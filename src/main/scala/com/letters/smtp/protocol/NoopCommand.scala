package com.letters.smtp.protocol

import com.letters.smtp.{Failure,Command,ResponseType,Success,SmtpSession}

import scala.util.parsing.combinator._

sealed trait NoopToken
object NoopToken {
  case object NOOP extends NoopToken
}

case object NoopCommand extends Command {
  import NoopToken._

  case object NoopParser extends RegexParsers {
    def noop = "(?i)noop".r ^^ (_ => NOOP)

    def expr = noop

    def apply(s: String): Either[NoSuccess,Unit] = parseAll(expr, s) match {
      case Success(_,_) => Right(())
      case failure: NoSuccess => Left(failure)
    }
  }

  def execute(input: String, session: SmtpSession): (ResponseType,SmtpSession) = NoopParser(input) match {
    case Right(_) =>
      (Success(250, "OK"), session)
    case Left(failure) =>
      (Failure(452, "ERROR"), session)
  }

  val verb = "noop"
}
