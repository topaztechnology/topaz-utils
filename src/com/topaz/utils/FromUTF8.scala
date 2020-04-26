package com.topaz.utils

import java.nio.ByteBuffer
import java.nio.charset.Charset

object FromUTF8 {

  val utf8charset = Charset.forName("UTF-8") // The UTF-8 character set used by FlatBuffers.

  private val fromUTF8Cache = Cache.createLimitedCache("FromUTF8.apply", maximumSize = 1000)
  def uncached(bb:ByteBuffer): String = {
    utf8charset.decode(bb.duplicate).toString 
  }

  def cached(bb: ByteBuffer): String = fromUTF8Cache.memoize(bb.duplicate){
    uncached(bb)
  }
}
