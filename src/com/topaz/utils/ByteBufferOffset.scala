package com.topaz.utils

case class ByteBufferOffset(bufferNo: Int, offset: Int, limit: Int) {
  /**
    * If this throws with offset and limit as 0 it can be because we are trying 
    * to load a tombstoned trade from the transaction DB. 
    * Tombstoned trades are stored with offset and limit set to 0.
    */
  require(offset < limit, s"Invalid offset ($offset) and limit ($limit)")
}

