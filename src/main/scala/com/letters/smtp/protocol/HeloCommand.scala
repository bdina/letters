package com.letters.smtp.protocol

import com.letters.smtp.{Command,Failure,ResponseType,SMTP,SmtpSession,Success}

import scala.util.parsing.combinator._

sealed trait HeloToken
object HeloToken {
  case class  HELO_HOST(host:String) extends HeloToken
  case object HELO                   extends HeloToken
}

case object HeloCommand extends Command {
  import HeloToken._

  case object HeloParser extends RegexParsers {
    def helo: Parser[HELO.type] = "(?i)helo".r     ^^ { _ => HELO }
    def host: Parser[HELO_HOST] = "[a-zA-Z_.-]+".r ^^ { str => HELO_HOST(str) }

    def expr: Parser[HELO_HOST] = helo ~ host      ^^ { case _ ~ host => host }

    def apply(s: String): Either[NoSuccess,String] = parseAll(expr, s) match {
      case Success(result,_) => Right(result.host)
      case failure: NoSuccess => Left(failure)
    }
  }

  def execute(input: String, session: SmtpSession): (ResponseType,SmtpSession) = HeloParser(input) match {
    case Right(address) =>
      (Success(250, "OK - helo"), session.withHost(address).withMode(SMTP))
    case Left(failure) =>
      (Failure(452, "ERROR"), session)
  }

  val verb = "helo"
}
