package com.topaz.utils

import java.lang.Thread.UncaughtExceptionHandler
import java.util.concurrent.ThreadFactory

case class NamedThreadFactory(
  name: String, daemon: Boolean, sequenced: Boolean, priority: Int = Thread.NORM_PRIORITY) extends ThreadFactory {

  private lazy val sequence = new java.util.concurrent.atomic.AtomicInteger(1)

  def newThread(r: Runnable): Thread = {
    val threadName = if(sequenced) s"$name ${sequence.getAndIncrement}" else name
    val t = new Thread(r, threadName)
    t.setPriority(priority)
    t.setDaemon(daemon)
    t.setUncaughtExceptionHandler(new UncaughtExceptionHandler {
      def uncaughtException(t: Thread, e: Throwable) {
        Console.err.println("uncaught exception", e)
      }
    })
    t
  }
}
