package io.github.edadma.smtp

import scala.collection.mutable.ArrayBuffer
import scala.io.Codec

class LineParser extends Machine:
  val start: State = commandState
  val line = new ArrayBuffer[Byte]

  def parseError: Nothing = sys.error("line parsing error")

  case object commandState extends State:
    override def enter(): Unit = line.clear()

    def on = {
      case '\r' => transition(eolState)
      case b    => line += b.toByte
    }

  case object eolState extends State:
    def on = {
      case '\n' => transition(FINAL)
      case _    => parseError
    }

  override def toString: String = new String(line.toArray, Codec.UTF8.charSet)
