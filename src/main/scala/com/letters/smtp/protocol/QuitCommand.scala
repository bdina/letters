package com.letters.smtp.protocol

import com.letters.smtp.{Close,Command,Failure,ResponseType,SmtpSession,LettersLogging}

import scala.util.parsing.combinator._

sealed trait QuitToken
object QuitToken {
  case object QUIT extends QuitToken
}

case object QuitCommand extends Command with LettersLogging {
  import QuitToken._

  case object QuitParser extends RegexParsers {
    def quit = "(?i)quit".r ^^ (_ => QUIT)

    def expr = quit

    def apply(s: String): Either[NoSuccess,Unit] = parseAll(expr, s) match {
      case Success(_,_) => Right(())
      case failure: NoSuccess => Left(failure)
    }
  }

  def execute(input: String, session: SmtpSession): (ResponseType,SmtpSession) = QuitParser(input) match {
    case Right(_) =>
      val response = session.msgHandler.done()
      val _session = session.clear()
      logger.info(s"session ${_session.id} quit (command response $response)")
      (Close(), _session)
    case Left(failure) =>
      (Failure(452, "ERROR"), session)
  }

  val verb = "quit"
}
