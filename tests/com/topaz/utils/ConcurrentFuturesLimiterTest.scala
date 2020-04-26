package com.topaz.utils

import akka.Done
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import com.topaz.utils.ConcurrentFuturesLimiter.ConcurrentLimitExceeded
import org.scalatest.time.{Millis, Seconds, Span}

import scala.concurrent.{Future, Promise}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ConcurrentFuturesLimiterTest extends AnyFunSuite with Matchers with ScalaFutures with Eventually {
  implicit val defaultPatience = PatienceConfig(timeout = Span(15, Seconds), interval = Span(50, Millis))

  test("concurrent limit") {
    val limiter = new ConcurrentFuturesLimiter(maxConcurrentFutures = 3)
    def f = Future.successful(Done)

    val p1 = Promise[Done]
    val f1 = limiter.call(p1.future)

    limiter.call(Promise[Done].future)
    limiter.call(Promise[Done].future)

    // too many running now. f4 should fail
    val p4 = Promise[Done]
    val f4 = limiter.call(p4.future)
    f4.failed.futureValue shouldBe ConcurrentLimitExceeded

    // complete p1, so we can now call some more
    p1.completeWith(f)
    f1.futureValue shouldBe Done
    
    eventually {
      limiter.numberRunning() shouldBe 2
    }

    // successfully called for another future
    limiter.call(f).futureValue shouldBe Done
  }
}
