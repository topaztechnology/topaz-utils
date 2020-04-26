package com.topaz.utils

import java.util.concurrent.atomic.AtomicInteger

import com.topaz.utils.ConcurrentFuturesLimiter.ConcurrentLimitExceeded

import scala.concurrent.Future

/**
  * After maxConcurrentFutures are running new calls will return a failed future.
  */
class ConcurrentFuturesLimiter(maxConcurrentFutures: Int) {
  private val running = new AtomicInteger(0)

  def call[T](f: => Future[T]): Future[T] = {
    // how many would be running if we ran this future
    val runningAfterThisFuture = running.incrementAndGet()
    val future = if (runningAfterThisFuture > maxConcurrentFutures) {
      // too many, fail.
      Future.failed(ConcurrentLimitExceeded)
    } else {
      FutureUtils.safe(f)
    }
    future.onComplete {
      _ =>
        running.decrementAndGet()
    }(scala.concurrent.ExecutionContext.Implicits.global)
    future
  }
  
  def numberRunning(): Int = running.get()

  override def toString: String = {
    s"Max of $maxConcurrentFutures concurrent running requests"
  }
}

object ConcurrentFuturesLimiter {

  case object ConcurrentLimitExceeded extends RuntimeException("Concurrent limit exceeded")

}
