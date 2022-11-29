package com.letters.smtp

trait MessageHandlerFactory {
  def create(): MessageHandler
}
object MessageHandlerFactory {
  case object DefaultHandlerFactory extends MessageHandlerFactory {
    def create(): MessageHandler = MessageHandler.DefaultHandler
  }
}
