package com.letters.smtp.protocol

import com.letters.smtp.{Failure,PipelineCommand,ResponseType,ESMTP,SmtpSession,LettersLogging,Success}

import scala.util.parsing.combinator._

sealed trait EhloToken
object EhloToken {
  case class  EHLO_HOST(host:String) extends EhloToken
  case object EHLO                   extends EhloToken
}

case object EhloCommand extends PipelineCommand with LettersLogging {
  import EhloToken._

  case object EhloParser extends RegexParsers {
    def ehlo: Parser[EHLO.type] = "(?i)ehlo".r     ^^ { _ => EHLO }
    def host: Parser[EHLO_HOST] = "[a-zA-Z_.-]+".r ^^ { str => EHLO_HOST(str) }

    def expr: Parser[EHLO_HOST] = ehlo ~ host      ^^ { case _ ~ host => host }

    def apply(s: String): Either[NoSuccess,String] = parseAll(expr, s) match {
      case Success(result,_) => Right(result.host)
      case failure: NoSuccess => Left(failure)
    }
  }

  def execute(input: String, session: SmtpSession): (ResponseType,SmtpSession) = EhloParser(input) match {
    case Right(remote) =>
      session.channel.map { case channel =>
        val msgBuf = new StringBuilder()
        msgBuf.append(s"250-${java.net.InetAddress.getLocalHost.getHostName} greets $remote")
              .append("\r\n")
              .append("250-PIPELINING")
              .append("\r\n")
              .append("250-8BITMIME")
              .append("\r\n")
        val msg = msgBuf.toString
        channel.write(java.nio.ByteBuffer.wrap((msg.getBytes)))
      }
      val _session = session.clear().withHost(remote).withMode(ESMTP)
      logger.info(s"received ehlo from $remote :: session: ${_session}")
      (Success(250, "OK"), _session)
    case Left(failure) =>
      (Failure(452, "ERROR"), session)
  }

  val verb = "ehlo"
}
