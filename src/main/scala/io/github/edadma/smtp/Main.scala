package io.github.edadma.smtp

import io.github.spritzsn.libuv.*
import io.github.spritzsn.async.*

import java.nio.file.Paths
import scala.collection.immutable.VectorMap
import scala.collection.mutable
import scala.concurrent.Future
import scala.io.Codec
import scala.util.{Failure, Success}

@main def run(hostname: String, port: Int): Unit = server(hostname, port, 0, 4096)
