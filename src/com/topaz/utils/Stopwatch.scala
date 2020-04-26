package com.topaz.utils

import java.lang.management.ManagementFactory
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import akka.actor.{Cancellable, Scheduler}

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

case class Stopwatch(var startTime: Long = System.nanoTime, private var startCpuTime: Long = Stopwatch.mxBean.getCurrentThreadCpuTime()) extends Log {
  def currentTime(): Long = System.nanoTime
  def currentCpuTime(): Long = {
    Stopwatch.mxBean.getCurrentThreadCpuTime()
  }

  def restart() {
    startTime = currentTime()
    startCpuTime = currentCpuTime()
  }
  def start(): Unit = restart() 
  def printElapsed(): Unit = {
    val elapsed = Stopwatch.formatMs(elapsedMs())
    println("Elapsed time: " + elapsed)
  }
  def elapsedNanos(): Long = currentTime() - startTime
  def elapsedCpuNanos(): Long = currentCpuTime() - startCpuTime
  def elapsedMs(): Long = elapsedNanos() / 1000 / 1000
  def elapsedCpuMs(): Long = elapsedCpuNanos() / 1000 / 1000

  def elapsedString(): String = Stopwatch.formatMs(elapsedMs())

  override def toString: String = {
    val ms = Stopwatch.formatMs(elapsedMs())
    val cpu = Stopwatch.formatMs(elapsedCpuMs())
    s"Elapsed $ms, CPU $cpu"
  }

  def withTime[T](msg: String)(fn: => T): T = {
    println(s"Starting: $msg")
    val f = fn
    println(s"Finished: $msg: $this")
    f
  }

}

object Stopwatch extends Log with RichConcurrent {
  private val globalTimer = new MapBackedTimer("Global", recordTimes = false, reportCPUTime = true)

  private lazy val mxBean = ManagementFactory.getThreadMXBean()

  def formatNsAsMs(nanos: Long): String = {
    formatMs(nanos / 1000 / 1000)
  }

  def formatMs(milli: Long): String = {
    if (milli < 1000)
      milli + "(ms)"
    else if (milli < 60 * 1000)
      (milli / 1000) + "(s) " + (milli % 1000) + "(ms)"
    else
      (milli / (60 * 1000)) + "(m) " + ((milli / 1000) % 60) + "(s)"
  }

  def withTime[T](msg: String)(fn: => T): T = {
    Stopwatch().withTime(msg)(fn)
  }

  def countTime[T](name: String)(fn: => T): T = {
    globalTimer.countTime(name)(fn)
  }

  def startTimeCounting(): Unit = {
    globalTimer.startTimeCounting()
  }

  def pauseTimeCounting(): Unit = {
    globalTimer.pauseTimeCounting()
  }

  def clearTimeCounts(): Unit = {
    globalTimer.clearTimeCounts()
  }

  def reportTimes(): Unit = {
    globalTimer.reportTimes()
  }
}

trait CodeTimer {
  def countTime[T](funcName: String)(fn: => T): T

  def countFutureTime[T](funcName: String)(fn: => Future[T]): Future[T]

  def reportTimes(): Unit

  def startTimeCounting(): Unit

  def pauseTimeCounting(): Unit

  def clearTimeCounts(): Unit

  def startPrintingEvery(interval: FiniteDuration)(scheduler: Scheduler): Cancellable
}

object CodeTimer {
  val NullTimer: CodeTimer = new CodeTimer {
    def countTime[T](funcName: String)(fn: => T): T = fn

    def countFutureTime[T](funcName: String)(fn: => Future[T]): Future[T] = fn

    def reportTimes(): Unit = {}

    def startTimeCounting(): Unit = {}

    def pauseTimeCounting(): Unit = {}

    def clearTimeCounts(): Unit = {}

    def startPrintingEvery(interval: FiniteDuration)(scheduler: Scheduler): Cancellable = Cancellable.alreadyCancelled
  }
}

class MapBackedTimer(name: String, recordTimes: Boolean, reportCPUTime: Boolean) extends CodeTimer with Log with RichConcurrent {

  import Stopwatch._

  private val doTimeCounting = new AtomicBoolean(recordTimes)
  private val timeCounts = new AtomicReference(Map[String, (Long, Long, Long)]())

  def startPrintingEvery(interval: FiniteDuration)(scheduler: Scheduler): Cancellable = {
    scheduler.scheduleWithFixedDelay(interval, interval) {
      () => reportTimes()
    }(scala.concurrent.ExecutionContext.Implicits.global)
  }

  def countTime[T](funcName: String)(fn: => T): T = {
    if (doTimeCounting.get()) {
      val s = Stopwatch()
      val result = fn
      recordTime(s, funcName)
      result
    } else
      fn
  }

  def countAndReportTimes[T](funcName: String)(fn: => T): T = {
    val res = countTime(funcName)(fn)
    reportTimes()
    res
  }

  def countFutureTime[T](funcName: String)(fn: => Future[T]): Future[T] = {
    import scala.concurrent.ExecutionContext.Implicits.global

    val s = Stopwatch()
    val future = fn
    future.onComplete {
      _ =>
        recordTime(s, funcName)
    }
    future
  }

  private def recordTime(s: Stopwatch, funcName: String): Map[String, (Long, Long, Long)] = {
    timeCounts.transformAndGet(map => {
      val elapsedNanos = s.elapsedNanos()
      val elapsedCpuNanos = if (reportCPUTime) s.elapsedCpuNanos() else 0l
      val (e, eCpu, count) = map.getOrElse(funcName, (0l, 0l, 0l))
      map + (funcName -> (e + elapsedNanos, eCpu + elapsedCpuNanos, count + 1))
    })
  }

  def reportTimes(): Unit = {
    val timeCountSeq = timeCounts.get().toSeq

    if (timeCountSeq.nonEmpty) {
      val rows = timeCountSeq.sortWith(_._1 < _._1).map {
        case (funcName, (time, cpuTime, nCalls)) =>
          Seq(funcName, formatNsAsMs(time), formatNsAsMs(cpuTime), nCalls)
      }

      val cpuHeader = if (reportCPUTime) "CPU" else "CPU (not recorded)"
      val headers = Seq("Name", "Elapsed", cpuHeader, "Calls")

      val table = StringUtils.formatAsTable(Seq(headers) ++ rows)

      log.info(s"Stopwatch times for $name${StringUtils.localNewline}" + table)
    }
  }

  def startTimeCounting(): Unit = {
    doTimeCounting.set(true)
  }

  def pauseTimeCounting(): Unit = {
    doTimeCounting.set(false)
  }

  def clearTimeCounts(): Unit = {
    timeCounts.set(Map.empty)
  }
}
