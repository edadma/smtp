package io.github.edadma.smtp

import scala.collection.mutable.ArrayBuffer

class LineParser extends Machine:
  val start: State = commandState
  val line = new StringBuilder // todo: unicode? new ArrayBuffer[Byte]

  def parseError: Nothing = sys.error("line parsing error")

  case object commandState extends State: // todo: parse commands early for efficiency
    override def enter(): Unit = line.clear()
    override def exit(): Unit = if line.isEmpty then parseError

    def on = {
      case '\r' => transition(eolState)
      case b    => line += b.toChar
    }

  case object eolState extends State:
    def on = {
      case '\n' => transition(FINAL)
      case _    => parseError
    }
