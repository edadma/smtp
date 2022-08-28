package io.github.edadma.smtp

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Codec

class LineParser extends Machine:
  val start: State = lineState
  val line = new ArrayBuffer[Byte]
  val lines = new ListBuffer[ArraySeq[Byte]]

  def parseError: Nothing = sys.error("line parsing error")

  override def init(): Unit =
    line.clear()
    lines.clear()

  case object lineState extends State:
    def on = {
      case '\r' => transition(eolState)
      case b    => line += b.toByte
    }

  case object eolState extends State:
    def on = {
      case '\n' =>
        lines += line to ArraySeq
        line.clear()
        transition(lineState)
      case b =>
        line += b.toByte
        transition(lineState)
    }
