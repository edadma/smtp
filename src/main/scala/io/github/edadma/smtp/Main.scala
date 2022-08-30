package io.github.edadma.smtp

import io.github.spritzsn.libuv.*
import io.github.spritzsn.async.*

import java.nio.file.Paths
import scala.collection.immutable.VectorMap
import scala.collection.mutable
import scala.concurrent.Future
import scala.io.Codec
import scala.util.{Failure, Success}

@main def run(): Unit =
  val domain = "example.com"
  val server = defaultLoop.tcp
  val port = 3000
  val flags = 0
  val backlog = 4096
  var exceptionHandler: (Response, Throwable) => Unit =
    (res, ex) => res.send(s"500 exception '${ex.getClass}': ${ex.getMessage}")

  val provider = FSCarrierProvider(Paths.get("mailboxes"))

  val HELORegex = """HELO (.+)""".r
  val MAILRegex = """MAIL FROM:<(.+)>""".r
  val RCPTRegex = """RCPT TO:<(.+)@(.+)>""".r
  val NOOPRegex = "NOOP(?: (.+))?".r
  val HELPRegex = "HELP(?: (.+))?".r

//  implicit class RegexOps(sc: StringContext) {
//    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
//  }

  def exchange(
      parser: LineParser,
      handler: Carrier,
      state: Exchange,
  ): Future[Response] =
    val line = bytes2string(parser.lines.head)

    if state.state == ExchangeState.Command then
      if parser.lines.length == 1 then
        line match
          case HELORegex(domain)          => handler.hello(domain)
          case MAILRegex(email)           => handler.from(email)
          case RCPTRegex(mailbox, domain) => handler.to(mailbox, domain)
          case "DATA" =>
            state.state = ExchangeState.Headers
            Future(Response(354))
          case "RSET"       => handler.reset
          case NOOPRegex(_) => Future(Response(250))
          case HELPRegex(_) => Future(Response(214))
          case "QUIT" => Future(new Response(end = true).send(s"221 $domain Service closing transmission channel"))
          case _      => Future(Response(500))
      else Future(Response(503))
    else
      var i = 0

      while i < parser.lines.length do
        val line = bytes2string(parser.lines(i))

        if line == "." then
          if i == parser.lines.length - 1 then
            state.state = ExchangeState.Command
            return handler.message(state.headers to VectorMap, state.body.toString)
          else return Future(Response(503))
        else if state.state == ExchangeState.Headers then
          if line == "" then state.state = ExchangeState.Body
          else if line startsWith " " then
            state.headers.lastOption match
              case Some((k, v)) => state.headers(k) = v ++ " " ++ line.trim
              case None         => return Future(Response(501))
          else
            line indexOf ':' match
              case -1  => return Future(Response(501))
              case idx => state.headers(line.substring(0, idx)) = line.substring(idx + 1).trim
        else
          if state.body.nonEmpty then state.body += '\n'
          state.body ++= line

        i += 1
      end while

      Future(new Response())

  enum ExchangeState:
    case Command, Headers, Body

  class Exchange:
    var state: ExchangeState = ExchangeState.Command
    val headers = new mutable.LinkedHashMap[String, String]
    val body = new StringBuilder

  def connectionCallback(server: TCP, status: Int): Unit =
    if status < 0 then Console.err.println(s"connection error: ${strError(status)}")
    else
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

            if parser.state == parser.lineState then
              exchange(parser, handler.get, state) onComplete {
                case Success(res) =>
                  try respond(res, client)
                  catch case e: Exception => close(client)
                case Failure(e) =>
                  val res = new Response(end = true)

                  exceptionHandler(res, e)
                  respond(res, client)
              }
              parser.reset()
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

def bytes2string(bytes: collection.IndexedSeq[Byte]): String = new String(bytes.toArray, Codec.UTF8.charSet)
