package com.letters.smtp

trait LettersLogging {
  import java.util.logging.Logger
  val logger: Logger = Logger.getLogger(this.getClass.getName)
}

