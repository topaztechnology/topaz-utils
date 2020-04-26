package com.topaz.utils

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, OutputStream}
import java.util.zip.{Deflater, GZIPInputStream, GZIPOutputStream}

object GZip {
  class TopazGZIPOutputStream(out: OutputStream, level: Int) extends GZIPOutputStream(out) {
    `def`.setLevel(level)
  }

  def gzip(txt: String, level: Int = Deflater.BEST_SPEED): Array[Byte] = {
    val arrOutputStream = new ByteArrayOutputStream()
    val zipOutputStream = new TopazGZIPOutputStream(arrOutputStream, level)
    zipOutputStream.write(txt.getBytes("UTF-8"))
    zipOutputStream.close()
    arrOutputStream.toByteArray
  }

  def gunzip(bytes: Array[Byte]): String = {
    val stream = new ByteArrayInputStream(bytes)
    val zipStream = new GZIPInputStream(stream)
    val arrOutputStream = new ByteArrayOutputStream()
    val buffer = new Array[Byte](16384)
    var read = 0
    while (read >= 0) {
      read = zipStream.read(buffer)
      if(read > 0)
        arrOutputStream.write(buffer, 0, read)
    }
    zipStream.close()
    arrOutputStream.close()
    new String(arrOutputStream.toByteArray, "UTF-8")
  }
}
