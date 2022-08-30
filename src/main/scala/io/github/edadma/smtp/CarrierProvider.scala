package io.github.edadma.smtp

import scala.collection.immutable.VectorMap
import scala.concurrent.Future

import io.github.spritzsn.async.*

abstract class Carrier:
  def hello(domain: String): Future[Response]

  def from(sender: String): Future[Response]

  def to(recipient: String): Future[Response]

  def message(headers: VectorMap[String, String], body: String): Future[Response]

  def reset: Future[Response]

abstract class CarrierProvider:
  def handler: Option[Carrier]

object DebugCarrierProvider$ extends CarrierProvider:
  def handler: Option[Carrier] = Some(new DebugCarrier)

  class DebugCarrier extends Carrier:
    def hello(domain: String): Future[Response] =
      println(s"hello $domain")
      Future(Response(250))

    def from(sender: String): Future[Response] =
      println(s"from $sender")
      Future(Response(250))

    def to(recipient: String): Future[Response] =
      println(s"to $recipient")
      Future(Response(250))

    def message(headers: VectorMap[String, String], body: String): Future[Response] =
      println(s"message $headers \"$body\"")
      Future(Response(250))

    def reset: Future[Response] =
      Future(Response(250))
