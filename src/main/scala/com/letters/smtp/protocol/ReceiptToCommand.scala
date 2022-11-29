package com.letters.smtp.protocol

import com.letters.smtp.{Failure,PipelineCommand,ResponseType,Success,SmtpSession,LettersLogging}

import scala.util.parsing.combinator._

sealed trait ReceiptToToken
object ReceiptToToken {
  case class  RCPT_TO_ADDR(address:String) extends ReceiptToToken
}

case object ReceiptToCommand extends PipelineCommand with LettersLogging {
  import ReceiptToToken._

  case object ReceiptToParser extends RegexParsers {
    val rcpt_to : Parser[String] = "(?i)rcpt to:".r
    val open    : Parser[String] = "<"
    val close   : Parser[String] = ">"
    val address : Parser[String] = "[a-zA-Z_.@]+".r

    val expr : Parser[RCPT_TO_ADDR] = rcpt_to ~ open ~> address <~ close ^^ (s => RCPT_TO_ADDR(s))

    def apply(s : String): Either[NoSuccess,String] = parseAll(expr, s) match {
      case Success(result,_) => result match { case RCPT_TO_ADDR(address) => Right(address) }
      case failure : NoSuccess => Left(failure)
    }
  }

  def execute(input: String, session: SmtpSession): (ResponseType,SmtpSession) = ReceiptToParser.apply(input) match {
    case Right(address) =>
      val response = session.msgHandler.recipient(address)
      logger.info(s"accept recipient: $response :: session recipients: ${session.RCPT_TO} :: session: $session")
      (Success(250, "OK"), session.withRecipient(address))
    case Left(failure) =>
      (Failure(452, s"${failure.msg}"), session)
  }

  val verb = "rcpt to"
}
