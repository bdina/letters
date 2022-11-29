package com.letters.smtp

import java.net.{InetAddress, InetSocketAddress}
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}
import java.nio.channels.spi.SelectorProvider
import java.nio.charset.StandardCharsets
import java.util.UUID

import com.letters.smtp.protocol._

import scala.collection.mutable.{ArrayBuffer,HashMap,Queue}

sealed trait SocketEvent {
  def socket: SocketChannel
  def ops: Int
}
object SocketEvent {
  case class REGISTER(socket: SocketChannel, ops: Int) extends SocketEvent
  case class FLIP(socket: SocketChannel, ops: Int) extends SocketEvent
  case class CLOSE(socket: SocketChannel, ops: Int) extends SocketEvent
}

sealed trait ResponseType {
  def code: Int
  def msg: String
  def bytes: Array[Byte] = s"$code $msg\r\n".getBytes(StandardCharsets.UTF_8)
}
case class Success(code: Int, msg: String) extends ResponseType
case class Failure(code: Int, msg: String) extends ResponseType
case class Close(code: Int = 221, msg: String = "BYE") extends ResponseType

sealed trait Mode
case object SMTP  extends Mode
case object ESMTP extends Mode

sealed trait Address extends Any
object Address {
  case class Host(address: String) extends AnyVal with Address
  case class MailFrom(address: String) extends AnyVal with Address
  case class RcptTo(address: String) extends AnyVal with Address
}

import Address._
case class SmtpSession(msgHandler  : MessageHandler
                     , authHandler : AuthenticationHandler
                     , auth        : Boolean               = false
                     , id          : UUID                  = UUID.randomUUID
                     , MODE        : Option[Mode]          = None
                     , HOST        : Option[Host]          = None
                     , MAIL_FROM   : Option[MailFrom]      = None
                     , RCPT_TO     : ArrayBuffer[RcptTo]   = ArrayBuffer.empty
                     , channel     : Option[SocketChannel] = None
                     , buffer      : Queue[ResponseType]   = Queue.empty) {

  def clear(): SmtpSession = copy(MAIL_FROM=None, RCPT_TO=ArrayBuffer.empty)

  def withMode(mode: Mode) = copy(MODE=Some(mode))
  def withHost(host: String) = copy(HOST=Some(Host(host)))
  def withSender(sender: String) = copy(MAIL_FROM=Some(MailFrom(sender)))
  def withRecipient(recipient: String) = copy(RCPT_TO=RCPT_TO += RcptTo(recipient))

  def messageTransaction: Boolean = MAIL_FROM.isDefined

  override def toString (): String =
    s"Session [ id:$id , host:$HOST , mode:$MODE , mail from:$MAIL_FROM , rcpt to:$RCPT_TO ]"
}

case class ServerResponse(server: LettersServer
                        , socket: SocketChannel
                        , session: SmtpSession)

case class CommandRegistry() {

  type Verb = String

  private val registry: HashMap[Verb,Command] = HashMap.empty

  def register(cmd: Command): Unit = registry += cmd.verb -> cmd
  def command(verb: Verb): Option[Command] = registry.get(verb)
  def find(predicate: (Verb => Boolean)): Option[Verb] = registry.keys.find(predicate)
  def find(verb: Verb): Option[Command] = find(verb.startsWith(_)) match {
    case Some(key) => command(key)
    case _ => None
  }
}

case class Worker(server: LettersServer, cmdRegistry: CommandRegistry) extends Runnable with LettersLogging {
  import java.nio.channels.SocketChannel

  private val msgHandlerFactory = server.msgHandlerFactory
  private val authHandlerFactory = server.authHandlerFactory

  private val session_map: HashMap[SocketChannel,SmtpSession] = HashMap.empty
  private val queue: Queue[ServerResponse] = Queue.empty

  private def parse(socket: SocketChannel , data: Array[Byte]): List[ServerResponse] = {
    (new String(data, StandardCharsets.UTF_8).toLowerCase).split("\r\n").toList.map { case input =>
      val session = session_map.getOrElse(socket, SmtpSession(msgHandler=msgHandlerFactory.create(), authHandler=authHandlerFactory.create(), channel = Some(socket)))
      logger.info(s"-- input => $input -- buffered => ${session.buffer}")
      cmdRegistry.find(input) match {
        case Some(cmd @ AuthCommand) if !session.auth =>
          logger.info(s"AUTH (PIPELINE) COMMAND - $cmd")
          val (response,sess) = cmd.execute(input,session)
          session_map += socket -> sess
          sess.buffer.enqueue(response)
          logger.info(s"complete PIPELINE $cmd with buffer ${sess.buffer}")
          ServerResponse(server,socket,sess)
        case Some(cmd: PipelineCommand) if session.auth =>
          logger.info(s"PIPELINE COMMAND - $cmd")
          val (response,sess) = cmd.execute(input,session)
          session_map += socket -> sess
          sess.buffer.enqueue(response)
          logger.info(s"complete PIPELINE $cmd with buffer ${sess.buffer}")
          ServerResponse(server,socket,sess)
        case Some(cmd: Command) if session.auth =>
          logger.info(s"COMMAND - $cmd")
          while (session.buffer.nonEmpty) {
            val response = session.buffer.dequeue()
            logger.info(s"-- FLUSH BUFFERED RESPONSE -> $response --")
            session.channel.map { case channel => channel.write(java.nio.ByteBuffer.wrap(response.bytes)) }
          }
          val (response,_session) = cmd.execute(input,session)
          _session.buffer.enqueue(response)
          logger.info(s"complete $cmd with buffer ${_session.buffer}")
          ServerResponse(server,socket,_session)
        case unauthenticated if !session.auth =>
          logger.severe(s"USER NOT AUTHENTICATED - $unauthenticated")
          session.buffer.enqueue(Failure(553, "authentication required"))
          ServerResponse(server,socket,session)
        case err =>
          logger.severe(s"UNKNOWN COMMAND - $err")
          session.buffer.enqueue(Failure(553, "command not implemented"))
          ServerResponse(server,socket,session)
      }
    }
  }

  def processData(socket: SocketChannel, data: Array[Byte], count: Int): Unit =
    /**
     * array copy is inefficient - consider passing count through
     *  the array is coming from the read bytebuffer which is cleared on
     *  each command sent by client, but clearing the bytebuffer ONLY
     *  resets the position and leaves the data to be overwritten. this
     *  is a problem when a subsequent command is fewer bytes, which is
     *  why the code now copies the array coming from the bytebuffer
     *  so it will not pass along any overflow to the commands
     */
    parse(socket, Array.copyOf(data,count)).foreach { case response =>
      queue += response
      queue.notify()
    }

  def run(): Unit = {
    while (true) {
      val dataEvent = ({
        while (queue.isEmpty) { queue.wait() /* Wait for data to become available */ }
        queue.remove(0)
      })
      logger.info(s"send response for buffer ${dataEvent.session.buffer}")
      while (dataEvent.session.buffer.nonEmpty) {
        dataEvent.session.buffer.dequeue() match {
          case response @ Close(code,msg) =>
            logger.info("-- QUIT --")
            val bytes = "221 service closing transmission channel\r\n".getBytes(StandardCharsets.UTF_8)
            server.close(dataEvent.socket, bytes)
          case response @ Success(code,msg) =>
            logger.info(s"RESPONSE -> ${new String(response.bytes)} (bytes=${response.bytes.mkString(" ")})")
            server.send(dataEvent.socket, response.bytes)
          case response @ Failure(code,msg) =>
            server.send(dataEvent.socket, response.bytes)
        }
      }
    }
  }
}

// The host:port combination to listen on
case class LettersServer(val msgHandlerFactory: MessageHandlerFactory
                       , val authHandlerFactory: AuthenticationHandlerFactory
                       , port: Int = 2525
                       , hostAddress: Option[InetAddress] = None) extends Runnable with LettersLogging {

  import java.nio.ByteBuffer

  // The selector we'll be monitoring
  private val selector = initSelector()

  // The buffer into which we'll read data when it's available
  private val readBuffer = ByteBuffer.allocate(8192)

  // A list of PendingChange instances
  private val pendingSocketEvents: ArrayBuffer[SocketEvent] = ArrayBuffer.empty

  // Maps a SocketChannel to a list of ByteBuffer instances
  private val pendingData: HashMap[SocketChannel,Queue[ByteBuffer]] = HashMap.empty

  private val cmdRegistry = CommandRegistry()
  cmdRegistry.register(AuthCommand)
  cmdRegistry.register(EhloCommand)
  cmdRegistry.register(HeloCommand)
  cmdRegistry.register(MailFromCommand)
  cmdRegistry.register(ReceiptToCommand)
  cmdRegistry.register(DataCommand)
  cmdRegistry.register(ResetCommand)
  cmdRegistry.register(QuitCommand)
  cmdRegistry.register(NoopCommand)

  private val worker = Worker(this, cmdRegistry)

  private def initSelector(): Selector = {
    // Create a new selector
    val socketSelector = SelectorProvider.provider().openSelector()

    // Create a new non-blocking server socket channel
    val serverChannel = ServerSocketChannel.open()
    serverChannel.configureBlocking(false)

    // Bind the server socket to the specified address and port
    val isa = new InetSocketAddress(hostAddress.getOrElse(null), port)
    serverChannel.socket().bind(isa)

    // Register the server socket channel, indicating an interest in 
    // accepting new connections
    serverChannel.register(socketSelector, SelectionKey.OP_ACCEPT)

    socketSelector
  }

  private def accept(key: SelectionKey): Unit = {
    // For an accept to be pending the channel must be a server socket channel.
    val serverSocketChannel = key.channel().asInstanceOf[ServerSocketChannel]

    // Accept the connection and make it non-blocking
    val socketChannel = serverSocketChannel.accept()
    socketChannel.configureBlocking(false)

    logger.info(s"accept connection - $socketChannel")

    val hostname = InetAddress.getLocalHost.getHostName
    send(socketChannel, s"220 Greetings earthling, pleased to meet you, I'm ${hostname}\r\n".getBytes)

    // Register the new SocketChannel with our Selector, indicating
    // we'd like to be notified when there's data waiting to be read
    socketChannel.register(selector, SelectionKey.OP_READ)
  }

  def close(socket: SocketChannel, data: Array[Byte]): Unit = {
    send(socket, data)

    // Indicate we want the interest ops set changed
    pendingSocketEvents += SocketEvent.CLOSE(socket, SelectionKey.OP_WRITE)

    // Finally, wake up our selecting thread so it can make the required changes
    selector.wakeup()
  }

  def send(socket: SocketChannel, data: Array[Byte]): Unit = {
    // Indicate we want the interest ops set changed
    pendingSocketEvents += SocketEvent.FLIP(socket, SelectionKey.OP_WRITE)

    // And queue the data we want written
    val queue = pendingData.getOrElse(socket, {
      val queue = Queue.empty[ByteBuffer]
      pendingData += socket -> queue
      queue
    })
    queue += ByteBuffer.wrap(data)

    // Finally, wake up our selecting thread so it can make the required changes
    selector.wakeup()
  }

  private def read(key: SelectionKey): Unit = {
    import java.io.IOException

    val socketChannel = key.channel().asInstanceOf[SocketChannel]

    // Clear out our read buffer so it's ready for new data
    readBuffer.clear()

    // Attempt to read off the channel
    val numRead = try {
      socketChannel.read(readBuffer)
    } catch {
      case _ : IOException =>
        // The remote forcibly closed the connection, cancel
        // the selection key and close the channel.
        key.cancel()
        socketChannel.close()
        -1
    }

    if (numRead == -1) {
      // Remote entity shut the socket down cleanly. Do the
      // same from our end and cancel the channel.
      key.channel().close()
      key.cancel()
    } else {
      // Hand the data off to our worker thread
      worker.processData(socketChannel, readBuffer.array(), numRead)
    }
  }

  private def write(key: SelectionKey): Unit = {
    val socketChannel = key.channel().asInstanceOf[SocketChannel]

    val socket = socketChannel.socket

    val tcp_send_buffer_size = socket.getSendBufferSize
    val buf = ByteBuffer.allocate(tcp_send_buffer_size)

    val queue = pendingData(socketChannel)
    val queued_bytes = queue.foldLeft (0) { case (acc,buf) => acc + buf.remaining }

    val bytes_in_transit = socket.getInputStream.available
    logger.info(s"-- tcp send buffer bytes: $tcp_send_buffer_size -- bytes queued: $queued_bytes")
    if ((bytes_in_transit == 0)) {
      logger.info("-- flush buffer -- no command bytes in transit")
      while (queue.nonEmpty) {
        val response = queue.dequeue()
        logger.info(s"queued response: $response")
        buf.put(response.array)
      }
      buf.rewind()
      logger.info(s"write buffer to socket channel -> $buf")
      socketChannel.write(ByteBuffer.wrap(buf.array, 0, queued_bytes))
      buf.clear()
      logger.info("queue empty (no command bytes in transit) - switch socket to READ")
      key.interestOps(SelectionKey.OP_READ)
    } else {
      // Write until there's not more data ...
      while (queue.nonEmpty) {
        if (buf.remaining > queue.head.remaining) {
          val response = queue.dequeue()
          logger.info(s"-- buffer response -- command bytes in transit and tcp send buffer has capacity - $response")
          buf.put(response.array)
        } else {
          logger.info("-- flush buffer -- command bytes in transit and tcp send buffer full")
          buf.rewind()
          logger.info(s"write pipeline buffer to socket channel -> $buf")
          socketChannel.write(ByteBuffer.wrap(buf.array, 0, queued_bytes))
          buf.clear()
        }
      }

      buf.rewind()
      logger.info(s"write buffer to socket channel -> $buf")
      socketChannel.write(ByteBuffer.wrap(buf.array, 0, queued_bytes))
      buf.clear()

      // We wrote all data, so we're no longer interested in writing on this socket. Switch back to waiting for data.
      logger.info("queue empty (command bytes in transit) - switch socket to READ")
      if (queue.isEmpty) key.interestOps(SelectionKey.OP_READ)
    }
  }

  def run(): Unit = {
    while (true) {
      // Process any pending changes
      pendingSocketEvents.foreach { case event =>
        event match {
          case SocketEvent.REGISTER(_,_) =>
            None
          case SocketEvent.FLIP(_,_) =>
            Option(event.socket.keyFor(selector)).map { case key => key.interestOps(event.ops) }
          case SocketEvent.CLOSE(_,_) =>
            Option(event.socket.keyFor(selector)).map { case key =>
              write(key)
              event.socket.close()
            }
        }
      }
      pendingSocketEvents.clear()

      // Wait for an event one of the registered channels
      selector.select()

      // Iterate over the set of keys for which events are available
      val selectedKeys = selector.selectedKeys().iterator()
      while (selectedKeys.hasNext) {
        val key = selectedKeys.next()
        selectedKeys.remove()

        if (key.isValid) {
          // Check what event is available and deal with it
               if (key.isAcceptable) accept (key)
          else if (key.isReadable  ) read   (key)
          else if (key.isWritable  ) write  (key)
        }
      }
    }
  }

  def start(): Unit = {
    import java.util.concurrent.Executors
    import scala.concurrent.ExecutionContext

    val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(2))

    ec.execute(worker)
    ec.execute(this)

    logger.info("letters smtp server started")
  }
}

object LettersServer extends App {
  val server = LettersServer(msgHandlerFactory=MessageHandlerFactory.DefaultHandlerFactory
                           , authHandlerFactory=AuthenticationHandlerFactory.DefaultHandlerFactory)
  server.start()
}
