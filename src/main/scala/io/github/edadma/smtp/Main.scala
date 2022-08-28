package io.github.edadma.smtp

import io.github.spritzsn.libuv.*
import io.github.spritzsn.async.*

import scala.collection.mutable
import scala.concurrent.Future
import scala.util.{Failure, Success}

@main def run(): Unit =
  val domain = "example.com"
  val server = defaultLoop.tcp
  val port = 3000
  val flags = 0
  val backlog = 4096
  var exceptionHandler: (Response, Throwable) => Unit =
    (res, ex) => res.send(s"500 exception '${ex.getClass}': ${ex.getMessage}")

  val HELOr = """HELO\s+(.+)""".r
  val MAILr = """MAIL FROM:\s*(.+)""".r
  val RCPTr = """RCPT TO:\s*(.+)""".r

  def process(parser: LineParser, session: mutable.HashMap[String, String]): Future[Response] =
    val line = parser.line.toString

    if session contains "data" then Future(new Response())
    else
      val (res, end) =
        line match
          case HELOr(domain) =>
            session("domain") = domain
            "250 OK" -> false
          case MAILr(from) =>
            session("from") = from
            "250 OK" -> false
          case RCPTr(to) =>
            session("to") = to
            "250 OK" -> false
          case _ => "500 bad command" -> true

      Future(new Response(end = end).send(res))

  def connectionCallback(server: TCP, status: Int): Unit =
    val client = defaultLoop.tcp
    val parser = new LineParser
    val session = new mutable.HashMap[String, String]

    def readCallback(client: TCP, size: Int, buf: Buffer): Unit =
      if size < 0 then
        client.readStop
        if size != eof then println(s"error in read callback: ${errName(size)}: ${strError(size)}") // todo
      else if size > 0 then
        try
          var i = 0

          while i < size do
            parser send buf(i)
            i += 1

          if parser.isFinal then
            parser.reset()
            process(parser, session) onComplete {
              case Success(res) =>
                try respond(res, client)
                catch case e: Exception => close(client)
              case Failure(e) =>
                val res = new Response()

                exceptionHandler(res, e)
                respond(res, client)
            }
        catch case e: Exception => respond(new Response().send(s"500 ${e.getMessage}"), client)
    end readCallback

    server accept client
    client readStart readCallback
    respond(new Response().send(s"220 $domain Simple Mail Transfer Service Ready"), client)
  end connectionCallback

  class Response(var data: IndexedSeq[Byte] = null, var end: Boolean = false):
    def send(s: String): Response =
      data = (s ++ "\r\n").getBytes.toIndexedSeq
      this

  def close(client: TCP): Unit =
    client.readStop

    if client.isWritable then client.shutdown(_.close())
    else if client.isClosing then client.dispose()
    else client.close()

  def respond(res: Response, client: TCP): Boolean =
    if res.end then client.readStop

    if client.isWritable then
      if res.data != null then client.write(res.data)
      if res.end then client.shutdown(_.close())
      res.end
    else if client.isClosing then
      client.dispose()
      true
    else
      client.close()
      true

  server.bind("0.0.0.0", port, flags)
  server.listen(backlog, connectionCallback)
  loop.run()
