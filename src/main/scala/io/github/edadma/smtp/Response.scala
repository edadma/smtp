package io.github.edadma.smtp

class Response(val end: Boolean = false):
  var data: Array[Byte] = Array()

  def send(s: String): Response =
    data = (s ++ "\r\n").getBytes
    this

  override def toString: String = new String(data)

object Response:
  def apply(code: Int, end: Boolean = false): Response =
    val text =
      code match
        case 214 => "https://www.rfc-editor.org/rfc/rfc5321"
        case 250 => "OK"
        case 354 => "Start mail input; end with <CRLF>.<CRLF>"
        case 500 => "Syntax error, command unrecognized"
        case 501 => "Syntax error in parameters or arguments"
        case 502 => "Command not implemented"
        case 503 => "Bad sequence of commands"
        case 550 => "Requested action not taken: mailbox unavailable"
        case 552 => "Requested mail action aborted: exceeded storage allocation"
        case 554 => "No SMTP service here"

    new Response(end).send(s"$code $text")
