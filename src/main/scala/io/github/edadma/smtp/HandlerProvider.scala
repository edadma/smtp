package io.github.edadma.smtp

import scala.collection.immutable.VectorMap
import scala.concurrent.Future

import io.github.spritzsn.async.*

abstract class Handler:
  def hello(domain: String): Future[Response]

  def from(sender: String): Future[Response]

  def to(recipient: String): Future[Response]

  def message(headers: VectorMap[String, String], body: String): Future[Response]

  def reset: Future[Response]

abstract class HandlerProvider:
  def handler: Option[Handler]

object SimpleHandlerProvider extends HandlerProvider:
  def handler: Option[Handler] = Some(new SimpleHandler)

  class SimpleHandler extends Handler:
    def hello(domain: String): Future[Response] =
      println(s"hello $domain")
      Future(new Response().send("250 OK"))

    def from(sender: String): Future[Response] =
      println(s"from $sender")
      Future(new Response().send("250 OK"))

    def to(recipient: String): Future[Response] =
      println(s"to $recipient")
      Future(new Response().send("250 OK"))

    def message(headers: VectorMap[String, String], body: String): Future[Response] =
      println(s"message $headers \"$body\"")
      Future(new Response().send("250 OK"))

    def reset: Future[Response] =
      Future(new Response().send("250 OK"))
