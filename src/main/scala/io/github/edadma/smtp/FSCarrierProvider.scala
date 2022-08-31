package io.github.edadma.smtp

import scala.collection.immutable.VectorMap
import scala.concurrent.Future
import io.github.spritzsn.async.*
import io.github.spritzsn.libuv.{O_CREAT, O_WRONLY, S_IRUSR, S_IWUSR, getHostname, getTimeOfDay, hrTime}
import io.github.spritzsn.fs.writeFile

import java.nio.file.{Files, Path}
import java.time.{Instant, ZoneOffset}
import java.time.format.DateTimeFormatter
import scala.collection.mutable.ListBuffer

private val TIMESTAMP_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSSSS")

class FSCarrierProvider(mailboxes: Path, hostname: String) extends CarrierProvider:
  def handler: Option[Carrier] = Some(new FSCarrier(mailboxes, hostname))

class FSCarrier(mailboxes: Path, hostname: String) extends Carrier:
  val list = new ListBuffer[Path]

  def hello(domain: String): Future[Response] = Future(Response(250))

  def from(sender: String): Future[Response] = Future(Response(250))

  def to(mailbox: String, domain: String): Future[Response] =
    if domain != hostname then Future(Response(550))
    else
      val path = mailboxes resolve mailbox

      if !Files.exists(path) || !Files.isExecutable(path) then Future(Response(550))
      else
        list += path
        Future(Response(250))

  def message(headers: VectorMap[String, String], body: String): Future[Response] =
    val writes =
      for r <- list yield
        val (sec, usec) = getTimeOfDay
        val nanos = f"${hrTime % 1000}%03d"
        val now = Instant.ofEpochSecond(sec, usec)
        val timestamp = TIMESTAMP_FORMAT.format(now.atOffset(ZoneOffset.UTC))
        val filename = s"$timestamp-$nanos-$hostname--"
        val path = r resolve filename

        writeFile(
          path.toString,
          (headers map { case (k, v) => s"$k: $v" } mkString "\n") ++ "\n" ++ body,
          O_WRONLY | O_CREAT,
          S_IRUSR | S_IWUSR,
        )

    Future.sequence(writes) map (_ => Response(250))

  def reset: Future[Response] =
    list.clear()
    Future(Response(250))
