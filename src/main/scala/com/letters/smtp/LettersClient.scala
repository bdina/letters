package com.letters.smtp

import java.net.InetAddress
import java.nio.channels.SocketChannel

sealed trait ChangeRequest {
  def socket: SocketChannel
  def ops: Int
}
object ChangeRequest {
  case class REGISTER(socket: SocketChannel, ops: Int) extends ChangeRequest
  case class CHANGEOPS(socket: SocketChannel, ops: Int) extends ChangeRequest
}

object SmtpClient {
  sealed trait Command {
    def bytes = toString.getBytes
  }

  case class EHLO(host: String) extends Command {
    override def toString = s"EHLO $host\r\n"
  }
  case class HELO(host: String) extends Command {
    override def toString = s"HELO $host\r\n"
  }
  case class FROM(address: String) extends Command {
    override def toString = s"MAIL FROM:<$address>\r\n"
  }
  case class TO(address: String) extends Command {
    override def toString = s"RCPT TO:<$address>\r\n"
  }
  case class DATA(message: String) extends Command {
    override def toString = s"DATA\r\n$message\r\n.\r\n"
  }

  case object QUIT extends Command {
    override def toString = "QUIT\r\n"
  }
  case object NOOP extends Command {
    override def toString = "NOOP\r\n"
  }
}

object Session {
  case class SmtpSession(socket: SocketChannel
                       , sender: Option[String] = None
                       , recipient: Vector[String] = Vector.empty
                       , terminated: Boolean = false) {
    def withSender(from: String) = copy(sender = Some(from))
    def withRecipient(to: String) = copy(recipient = recipient :+ to)
    def terminate() = copy(terminated = true)
  }
}

case class LettersClient(hostAddress: InetAddress, port: Int) extends Runnable with LettersLogging {
  import java.io.IOException
  import java.net.InetSocketAddress
  import java.nio.ByteBuffer
  import java.nio.channels.{SelectionKey,Selector,SocketChannel}
  import java.nio.channels.spi.SelectorProvider

  import scala.collection.mutable.{HashMap,Queue}

  import SmtpClient._

  // The selector we'll be monitoring
  private val selector = initSelector()

  // The buffer into which we'll read data when it's available
  private val readBuffer = ByteBuffer.allocate(8192)

  // A list of PendingChange instances
  private val pendingChanges: Queue[ChangeRequest] = Queue.empty

  // Maps a SocketChannel to a list of ByteBuffer instances
  private val pendingData: HashMap[SocketChannel,Queue[ByteBuffer]] = HashMap.empty

  // Start a new connection
  val socket = initiateConnection()

  def send(cmd: Command): Unit = send(cmd.bytes)

  def send(data: Array[Byte]): Unit = {
    // And queue the data we want written
    val queue = pendingData.getOrElse(socket, {
      val queue = Queue.empty[ByteBuffer]
      pendingData += socket -> queue
      queue
    })
    queue += ByteBuffer.wrap(data)

    pendingChanges += ChangeRequest.CHANGEOPS(socket, SelectionKey.OP_WRITE)

    // Finally, wake up our selecting thread so it can make the required changes
    selector.wakeup()
  }

  def run(): Unit = {
    while (true) {
      // Process any pending changes
      pendingChanges.foreach { case change =>
        change match {
          case ChangeRequest.CHANGEOPS(socket,ops) =>
            socket.keyFor(selector).interestOps(ops)
          case ChangeRequest.REGISTER(socket,ops) =>
            socket.register(selector, ops)
        }
      }
      pendingChanges.clear()

      // Wait for an event one of the registered channels
      selector.select()

      // Iterate over the set of keys for which events are available
      val selectedKeys = selector.selectedKeys().iterator()
      while (selectedKeys.hasNext()) {
        val key = selectedKeys.next().asInstanceOf[SelectionKey]
        selectedKeys.remove()

        if (key.isValid()) {
          // Check what event is available and deal with it
               if (key.isConnectable()) finishConnection (key)
          else if (key.isReadable())     read             (key)
          else if (key.isWritable())     write            (key)
        }
      }
    }
  }

  private def read(key: SelectionKey): Unit = {
    val socketChannel = key.channel().asInstanceOf[SocketChannel]

    // Clear out our read buffer so it's ready for new data
    readBuffer.clear()

    // Attempt to read off the channel
    val numRead = try {
      socketChannel.read(readBuffer)
    } catch {
      case e: IOException =>
        // The remote forcibly closed the connection, cancel
        // the selection key and close the channel
        key.cancel()
        socketChannel.close()
        -1
    }

    if (numRead == -1) {
      // Remote entity shut the socket down cleanly. Do the
      // same from our end and cancel the channel
      key.channel().close()
      key.cancel()
    } else {
      // Handle the response
      handleResponse(socketChannel, readBuffer.array(), numRead)

      key.interestOps(SelectionKey.OP_WRITE)
    }
  }

  private def handleResponse(socketChannel: SocketChannel, data: Array[Byte], numRead: Int): Unit = {
    // Make a correctly sized copy of the data before handing it to the client
    val rspData = new Array[Byte](numRead)
    Array.copy(data, 0, rspData, 0, numRead)

    logger.info(new String(rspData))

    //        // And pass the response to it
    //        if (handler.handleResponse(rspData)) {
    //            // The handler has seen enough, close the connection
    //            socketChannel.close()
    //            socketChannel.keyFor(selector).cancel()
    //        }
  }

  private def write(key: SelectionKey): Unit = {
    val socketChannel = key.channel().asInstanceOf[SocketChannel]

    val queue = pendingData(socketChannel)

    // Write until there's not more data ...
    import util.control.Breaks._
    breakable {
      while (queue.nonEmpty) {
        val buf = queue.dequeue()
        socketChannel.write(buf)
        if (buf.remaining() > 0) {
          // ... or the socket's buffer fills up
          break()
        }
      }
    }

    if (queue.isEmpty) {
      // We wrote away all data, so we're no longer interested
      // in writing on this socket. Switch back to waiting for
      // data
      key.interestOps(SelectionKey.OP_READ)
    }
  }

  private def finishConnection(key: SelectionKey): Unit = {
    val socketChannel = key.channel().asInstanceOf[SocketChannel]

    // Finish the connection. If the connection operation failed
    // this will raise an IOException
    val finished = try {
      socketChannel.finishConnect()
    } catch {
      case e: IOException =>
        // Cancel the channel's registration with our selector
        System.out.println(e)
        key.cancel()
        false
    }

    // Register an interest in writing on this channel
    if (finished) key.interestOps(SelectionKey.OP_WRITE)
  }

  private def initiateConnection(): SocketChannel = {
    // Create a non-blocking socket channel
    val socketChannel = SocketChannel.open()
    socketChannel.configureBlocking(false)

    // Kick off connection establishment
    socketChannel.connect(new InetSocketAddress(hostAddress, port))

    // Queue a channel registration since the caller is not the
    // selecting thread. As part of the registration we'll register
    // an interest in connection events. These are raised when a channel
    // is ready to complete connection establishment.
    pendingChanges += ChangeRequest.REGISTER(socketChannel, SelectionKey.OP_CONNECT)

    socketChannel
  }

  private def initSelector(): Selector = SelectorProvider.provider().openSelector()
}
object LettersClient extends App {
  import SmtpClient._

  val client = LettersClient(InetAddress.getByName("127.0.0.1"), 2525)

  val t = new Thread(client)
  t.setDaemon(true)
  t.start()

  client.send(HELO("foo.com"))
  client.send(FROM("foo@bar.com"))
//  client.send(TO("foo@baz.com"))
//  client.send(QUIT)
  while(true) Thread.sleep(1000)
}
