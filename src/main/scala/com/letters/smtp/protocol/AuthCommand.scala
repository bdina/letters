package com.letters.smtp.protocol

import com.letters.smtp.{Failure,PipelineCommand,ResponseType,Success,SmtpSession,LettersLogging}

import scala.util.parsing.combinator._

sealed trait AuthToken
object AuthToken {
  case object AUTH                       extends AuthToken
  case class MECHANISM(mechanism:String) extends AuthToken
  case class CIPHERTEXT(text:String)     extends AuthToken
}

case object AuthCommand extends PipelineCommand with LettersLogging {
  import AuthToken._

  case object AuthParser extends RegexParsers {
    val authcmd:    Parser[AUTH.type]  = "(?i)auth".r     ^^ (_ => AUTH)
    val mechanism:  Parser[MECHANISM]  = "[a-zA-Z_.@]+".r ^^ (s => MECHANISM(s))
    val ciphertext: Parser[CIPHERTEXT] = "[a-zA-Z_.@]+".r ^^ (s => CIPHERTEXT(s))

    def expr: Parser[(MECHANISM,CIPHERTEXT)] = authcmd ~ mechanism ~ ciphertext ^^ { case _ ~ m ~ c => (m,c) }

    def apply(s: String): Either[NoSuccess,(MECHANISM,CIPHERTEXT)] = parseAll(expr, s) match {
      case Success(result,_) => result match { case (m,c) => Right((m,c)) }
      case failure: NoSuccess => Left(failure)
    }
  }

  def execute(input: String, session: SmtpSession): (ResponseType,SmtpSession) = AuthParser(input) match {
    case Right((m,c)) if session.authHandler.auth(c.text) =>
      (Success(235, "2.7.0 Authentication successful"), session.copy(auth = true))
    case Right((m,c)) =>
      (Failure(452, "ERROR"), session)
    case Left(failure) =>
      (Failure(452, "ERROR"), session)
  }

  val verb = "auth"
}
