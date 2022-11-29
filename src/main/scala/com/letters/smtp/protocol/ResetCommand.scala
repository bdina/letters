package com.letters.smtp.protocol

import com.letters.smtp.{Failure,PipelineCommand,ResponseType,Success,SmtpSession}

import scala.util.parsing.combinator._

sealed trait ResetToken
object ResetToken {
  case object RESET extends ResetToken
}

case object ResetCommand extends PipelineCommand {
  import ResetToken._

  case object ResetParser extends RegexParsers {
    def rset = "(?i)rset".r ^^ (_ => RESET)

    def expr = rset

    def apply(s: String): Either[NoSuccess,Unit] = parseAll(expr, s) match {
      case Success(_,_) => Right(())
      case failure: NoSuccess => Left(failure)
    }
  }

  def execute(input: String, session: SmtpSession): (ResponseType,SmtpSession) = ResetParser(input) match {
    case Right(_) =>
      (Success(250, "OK"), session.clear())
    case Left(failure) =>
      (Failure(452, "ERROR"), session)
  }

  val verb = "rset"
}
