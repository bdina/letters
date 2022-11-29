package com.letters.smtp

trait Command {
  def execute(input: String, state: SmtpSession): (ResponseType,SmtpSession)
  def verb: String
}

trait PipelineCommand extends Command
