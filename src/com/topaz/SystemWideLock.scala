package com.topaz

import java.io.File

import com.topaz.utils.Log

object SystemWideLock extends Log {

  def canHoldSystemLock(file: File): Boolean = {
    // https://stackoverflow.com/a/2002948/190452
    import java.io.RandomAccessFile
    try {
      val randomAccessFile = new RandomAccessFile(file, "rw")
      val fileLock = randomAccessFile.getChannel.tryLock

      if (fileLock != null) {
        // It doesn't matter if this shutdown hook isn't called (e.g. if the jvm is killed
        // or crashes) as the tryLock will be released on the jvm exiting and we'll just end
        // up with the lock file lying around but not locked.
        Runtime.getRuntime.addShutdownHook(new Thread() {
          override def run(): Unit = {
            try {
              fileLock.release()
              randomAccessFile.close()
              file.delete
            } catch {
              case e: Exception =>
                log.error("Unable to remove lock file: " + file, e)
            }
          }
        })
      }

      fileLock != null
    } catch {
      case e: Exception =>
        log.error("Unable to create and/or lock file: " + file, e)
        false
    }
  }
}
