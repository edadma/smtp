package io.github.edadma.smtp

import scala.collection.immutable.VectorMap
import scala.concurrent.Future
import io.github.spritzsn.async.*
import io.github.spritzsn.libuv.{O_CREAT, O_WRONLY, S_IRUSR, S_IWUSR, getHostname, getTimeOfDay, hrTime}
import io.github.spritzsn.fs.writeFile

import java.nio.file.Path
import java.time.{Instant, ZoneOffset}
import java.time.format.DateTimeFormatter
import scala.collection.mutable.ListBuffer

private val TIMESTAMP_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSSSS")

class FSCarrier(mail: Path) extends Carrier:
  val list = new ListBuffer[String]

  def hello(domain: String): Future[Response] = Future(Response(250))

  def from(sender: String): Future[Response] = Future(Response(250))

  def to(mailbox: String, domain: String): Future[Response] =
    list += mailbox
    Future(Response(250))

  def message(headers: VectorMap[String, String], body: String): Future[Response] =
    val writes =
      for r <- list yield
        val (sec, usec) = getTimeOfDay
        val nanos = f"${hrTime % 1000}%03d"
        val now = Instant.ofEpochSecond(sec, usec)
        val timestamp = TIMESTAMP_FORMAT.format(now.atOffset(ZoneOffset.UTC))
        val filename = s"$timestamp-$nanos-$getHostname--"
        val path = mail resolve r resolve filename

        writeFile(path.toString, body, O_WRONLY | O_CREAT, S_IRUSR | S_IWUSR)

    Future.sequence(writes) map (_ => Response(250))

  def reset: Future[Response] =
    list.clear()
    Future(Response(250))
