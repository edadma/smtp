package io.github.edadma.smtp

class Response(var data: IndexedSeq[Byte] = null, var end: Boolean = false):
  def send(s: String): Response =
    data = (s ++ "\r\n").getBytes.toIndexedSeq
    this

  override def toString: String = new String(data.toArray)
