package io.github.edadma.smtp

import io.github.spritzsn.libuv.*
import io.github.spritzsn.async.*

import scala.collection.immutable.VectorMap
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

  val provider = SimpleHandlerProvider

  val HELOr = """HELO (.+)""".r
  val MAILr = """MAIL FROM:(.+)""".r
  val RCPTr = """RCPT TO:(.+)""".r

  def exchange(
      parser: LineParser,
      handler: Handler,
      state: Exchange,
  ): Future[Response] =
    val line = parser.toString

    if state.state == ExchangeState.Command then
      line match
        case HELOr(domain) => handler.hello(domain)
        case MAILr(from)   => handler.from(from)
        case RCPTr(to)     => handler.to(to)
        case "DATA" =>
          state.state = ExchangeState.Headers
          Future(new Response().send("354 Start mail input, end with <CRLF>.<CRLF>"))
        case "QUIT" => Future(new Response(end = true).send(s"221 $domain Service closing transmission channel"))
        case _      => Future(new Response(end = true).send("500 bad command"))
    else if line == "." then
      state.state = ExchangeState.Command
      handler.message(state.headers to VectorMap, state.body.toString)
    else if state.state == ExchangeState.Headers then
      if line == "" then
        state.state = ExchangeState.Body
        Future(new Response())
      else if line startsWith " " then
        state.headers.lastOption match
          case Some((k, v)) =>
            state.headers(k) = v ++ " " ++ line.trim
            Future(new Response())
          case None => Future(new Response(end = true).send("500 continued header without previous header line"))
      else
        line indexOf ':' match
          case -1 => Future(new Response(end = true).send("500 bad header"))
          case idx =>
            state.headers(line.substring(0, idx)) = line.substring(idx + 1).trim
            Future(new Response())
    else
      if state.body.nonEmpty then state.body += '\n'
      state.body ++= line
      Future(new Response())

  enum ExchangeState:
    case Command, Headers, Body

  class Exchange:
    var state: ExchangeState = ExchangeState.Command
    val headers = new mutable.LinkedHashMap[String, String]
    val body = new StringBuilder

  def connectionCallback(server: TCP, status: Int): Unit =
    val client = defaultLoop.tcp
    val parser = new LineParser
    val handler = provider.handler
    val state = new Exchange

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
            exchange(parser, handler.get, state) onComplete {
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
    // todo: handler could be None
    respond(new Response().send(s"220 $domain Simple Mail Transfer Service Ready"), client)
  end connectionCallback

  server.bind("0.0.0.0", port, flags)
  server.listen(backlog, connectionCallback)
  loop.run()

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
