package com.letters.smtp

trait AuthenticationHandler {
  def auth(input: String): Boolean
  def identity: String
}
object AuthenticationHandler {
  case object DefaultHandler extends AuthenticationHandler with LettersLogging {
    def auth(input: String): Boolean = true
    def identity: String = "foo"
  }
}
