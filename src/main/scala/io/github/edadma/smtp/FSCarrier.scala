package io.github.edadma.smtp

import scala.collection.immutable.VectorMap
import scala.concurrent.Future
import io.github.spritzsn.async.*
import io.github.spritzsn.libuv.{hrTime, getTimeOfDay, getHostname}

import java.nio.file.Path
import java.time.{Instant, ZoneOffset}
import java.time.format.DateTimeFormatter

private val TIMESTAMP_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSSSS")

class FSCarrier(mail: Path) extends Carrier:
  def hello(domain: String): Future[Response] = Future(Response(250))

  def from(sender: String): Future[Response] = Future(Response(250))

  def to(recipient: String): Future[Response] = Future(Response(250))

  def message(headers: VectorMap[String, String], body: String): Future[Response] =
    val (sec, usec) = getTimeOfDay
    val nanos = f"${hrTime % 1000}%03d"
    val now = Instant.ofEpochSecond(sec, usec)
    val timestamp = TIMESTAMP_FORMAT.format(now.atOffset(ZoneOffset.UTC))
    val filename = s"$timestamp-$nanos-$getHostname--"

    Future(Response(250))

  def reset: Future[Response] = Future(Response(250))
