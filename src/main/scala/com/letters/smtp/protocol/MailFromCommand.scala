package com.letters.smtp.protocol

import com.letters.smtp.{Failure,PipelineCommand,ResponseType,Success,SmtpSession,LettersLogging}

import scala.util.parsing.combinator._

sealed trait MailFromToken
object MailFromToken {
  case class  MAIL_FROM_ADDR(address:String) extends MailFromToken
}

case object MailFromCommand extends PipelineCommand with LettersLogging {
  import MailFromToken._

  case object MailFromParser extends RegexParsers {
    val mail_from : Parser[String] = "(?i)mail from:".r
    val open      : Parser[String] = "<"
    val close     : Parser[String] = ">"
    val address   : Parser[String] = "[a-zA-Z_.@]+".r

    val expr: Parser[MAIL_FROM_ADDR] = mail_from ~ open ~> address <~ close ^^ (s => MAIL_FROM_ADDR(s))

    def apply(s: String): Either[NoSuccess,String] = parseAll(expr, s) match {
      case Success(result,_) => result match { case MAIL_FROM_ADDR(address) => Right(address) }
      case failure : NoSuccess => Left(failure)
    }
  }

  def execute(input: String, session: SmtpSession): (ResponseType,SmtpSession) = MailFromParser(input) match {
    case Right(address) =>
      val response = session.msgHandler.from(address)
      val _session = session.withSender(address)
      logger.info(s"accept sender: $response :: session sender: ${session.MAIL_FROM} :: session: ${_session}")
      (Success(250, "OK - mail from"), _session)
    case Left(failure) =>
      (Failure(452, s"${failure.msg}"), session)
  }

  val verb = "mail from"
}
