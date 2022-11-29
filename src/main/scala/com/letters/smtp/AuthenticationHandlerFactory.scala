package com.letters.smtp

trait AuthenticationHandlerFactory {
  def create(): AuthenticationHandler
}
object AuthenticationHandlerFactory {
  case object DefaultHandlerFactory extends AuthenticationHandlerFactory {
    def create(): AuthenticationHandler = AuthenticationHandler.DefaultHandler
  }
}
