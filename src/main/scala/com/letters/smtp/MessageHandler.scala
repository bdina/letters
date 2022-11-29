package com.letters.smtp

import java.io.InputStream

trait MessageHandler {
  def from(address: String): ResponseType
  def recipient(address: String): ResponseType
  def data(is: InputStream): ResponseType
  def done(): ResponseType
}
object MessageHandler {
  case object DefaultHandler extends MessageHandler with LettersLogging {
    import java.io.InputStream
  
    def from(address: String): ResponseType = {
      logger.info(s"MAIL FROM: $address")
      Success(250, "OK")
    }
    def recipient(address: String): ResponseType = {
      logger.info(s"RCPT TO: $address")
      Success(250, "OK")
    }
    def data(is: InputStream): ResponseType = {
      val raw_data = new String(is.readAllBytes)
      logger.info(s"DATA:\n$raw_data")
      Success(250, "OK")
    }
    def done(): ResponseType = {
      logger.info("QUIT")
      Close()
    }
  }
}
