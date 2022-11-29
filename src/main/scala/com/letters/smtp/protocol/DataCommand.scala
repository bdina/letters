package com.letters.smtp.protocol

import com.letters.smtp.{Command,Failure,ResponseType,SmtpSession,LettersLogging,Success}

import java.io.ByteArrayInputStream
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import java.nio.charset.StandardCharsets

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._
import scala.util.parsing.combinator._

sealed trait DataToken
object DataToken {
  case class  MESSAGE(content:Array[Byte]) extends DataToken
  case object DATA                         extends DataToken
}

case object DataCommand extends Command with LettersLogging {
  import DataToken._

  case object DataParser extends RegexParsers {
    def expr: Parser[DATA.type] = "(?i)data".r ^^ (_ => DATA)

    def apply(s: String): Either[NoSuccess,Unit] = parseAll(expr, s) match {
      case Success(_,_) => Right(())
      case failure: NoSuccess => Left(failure)
    }
  }

  case class SocketChannelCharSequence(channel: SocketChannel) extends CharSequence with LettersLogging {

    val BUFFER_SIZE = channel.getOption(java.net.StandardSocketOptions.SO_RCVBUF)

    val buffer = ByteBuffer.allocate(BUFFER_SIZE)
    val data   = ArrayBuffer[Byte]()

    logger.info(s"SocketChannelCharSequence.apply - buffer:$BUFFER_SIZE")

    def read(): Unit = {
      val bytes = channel.read(buffer)
      if (bytes > 0) {
        logger.info(s"read $bytes bytes")
        val buff = Array.copyOf(buffer.array, bytes)
        buff.foreach(data += _)
        buffer.clear()
      }
    }

    logger.info(s"SocketChannelCharSequence.apply - read")
    read()

    def length(): Int = {
      read()
      data.size
    }

    def subSequence(start: Int, end: Int): CharSequence = {
      if (end > start) {
        read()
        new String(data.slice(start,end).toArray, StandardCharsets.UTF_8)
      } else {
        ""
      }
    }

    def charAt(index: Int): Char = {
      if (index > 0) {
        read()
        data(index).toChar
      } else {
        ' '
      }
    }
  }

  case object MessageParser extends RegexParsers with LettersLogging {
    import Array._

    /*
     * By default Scala parsers automatically ignore all whitespace, including EOLs. Considering that
     * regexp '.*' without any flags does not include EOLs, the line parser naturally means "the whole line until the
     * line break", so you don't have to analyze EOLs at all.
     *
     * Secondly, the msg parser as defined is greedy, it will happily consume everything including the final
     * '\r\n.\r\n'. To make it stop before eod we use the not combinator.
     *
     * We override the behaviour of skipping the whitespace, and analyze all whitespace manually. This includes the
     * whitespace after BEGIN and after the '\r\n.\r\n':
     *
     * Note: that line is defined as '.*' instead of '.+' here, as such the parser won't fail if there are any empty
     * lines in the input.
     */

    override def skipWhitespace = false

    val eol  : Parser[String]      = "\n" | "\r\n" | "\r"
    val line : Parser[String]      = """.*""".r
    val msg  : Parser[Seq[String]] = rep(not(eod) ~> line <~ eol)
    val eod  : Parser[String]      = "\r\n.\r\n"

    val expr : Parser[MESSAGE] =
      msg <~ eod ^^ (s => MESSAGE(s.foldLeft(Array.empty[Byte])((x,y) => concat(x,(y+"\r\n").getBytes))))

    def apply(channel: SocketChannel): MESSAGE = {
      logger.info("server: tell client to begin sending data")
      channel.write(ByteBuffer.wrap("354 End data with <CR><LF>.<CR><LF>\r\n".getBytes))
      var msg = MESSAGE(Array.empty)
      val sccs = new SocketChannelCharSequence(channel)
      breakable {
        while (true) {
          val response = parseAll(expr, sccs)
          logger.info(s"client: $response")
          if (response.successful) { msg = response.get ; break() } else { break() }
        }
      }
      msg
    }
  }

  def execute(input: String, session: SmtpSession): (ResponseType,SmtpSession) = {
    if (session.MAIL_FROM.isEmpty) return (Failure(503, "Error: need MAIL command"), session)
    if (session.RCPT_TO.isEmpty  ) return (Failure(503, "Error: need RCPT command"), session)

    logger.info(s"data command for session: $session with buffer: ${session.buffer}")

    DataParser(input) match {
      case Right(_) =>
        logger.info("begin DATA fetch...")
        session.channel.fold [(ResponseType,SmtpSession)] ((Failure(452, "ERROR"),session)) { case channel =>
          logger.info("parse payload...")
          val data_array = MessageParser(channel).content
          logger.info("call data handler API with raw email contents...")
          session.msgHandler.data(new ByteArrayInputStream(data_array))
          (Success(250, "OK"), session)
        }
      case Left(failure) =>
        logger.severe("command parse error")
        (Failure(452, "ERROR"), session)
    }
  }

  val verb = "data"
}
