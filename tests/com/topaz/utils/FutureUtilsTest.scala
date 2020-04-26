package com.topaz.utils

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import java.util.concurrent.{CountDownLatch, TimeUnit}

import akka.actor.ActorSystem
import com.topaz.utils
import com.topaz.utils.FutureUtils.LazyConditionalFuture
import org.scalactic.source
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.tagobjects.Slow
import org.scalatest.time.{Millis, Seconds, Span}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, Promise}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FutureUtilsTest extends AnyFunSuite with Matchers with ScalaFutures with FutureUtils {

  test("FutureFirstAndLastQueue drops specific work without running it") {
    implicit val system  = ActorSystem("FutureUtilsTest_queue")
    import scala.concurrent.ExecutionContext.Implicits.global
    val queue = new utils.FutureUtils.FutureFirstAndLastQueue()

    val count = new AtomicInteger(0)
    val runCount = new AtomicInteger(0)
    val doneSignal = new CountDownLatch(1)


    // first job, so will be run
    queue.add(() => {
      val future = Future {
        Thread.sleep(1000)        
        count.incrementAndGet()
      }
      future.onComplete(_ => runCount.incrementAndGet())
      future
    })

    // as each job is queued the previous one will be dropped because the first job will still be
    // running. job 100 will stay on the queue until below when the final job is added.
    val dropppedJobs = (1 to 100).par.map {
      _ =>
        queue.add(() => {
          val future = Future {
            count.incrementAndGet()
          }
          future.onComplete(_ => runCount.incrementAndGet())
          future
        })
    }

    // final job, adding it will remove job 100 from above, and it will be run once
    // the very first job finishes.
    queue.add(() => {
      val future = Future {
        count.incrementAndGet()
      }
      future.onComplete { _ =>
        runCount.incrementAndGet()
        doneSignal.countDown()
      }
      future
    })

    doneSignal.await(10, TimeUnit.SECONDS)
    count.get() shouldEqual 2
    runCount.get() shouldEqual 2
    
    dropppedJobs.toVector.foreach {
      job =>
        job.isCompleted shouldBe true
        job.failed.futureValue shouldBe FutureUtils.ResultNotNeededException
    }
  }
  
  test("FutureFirstAndLastQueue handles exceptions being thrown in the work") {
    implicit val system  = ActorSystem("FutureUtilsTest_queue")
    import scala.concurrent.ExecutionContext.Implicits.global
    val queue = new utils.FutureUtils.FutureFirstAndLastQueue()
    val count = new AtomicInteger(0)

    val doneSignal = new CountDownLatch(1)

    queue.add(() => {
      throw new Exception("Ignore in test output")
    })
    
    // queue should still be able to process new work
    queue.add(() => {
      val future = Future {count.incrementAndGet()}
      future.onComplete { _ =>
        doneSignal.countDown()
      }
      future
    })

    doneSignal.await(10, TimeUnit.SECONDS)
    count.get() shouldEqual 1
  }
  
  test("LazyConditionalFuture should not run if the condition is false") {
    import scala.concurrent.ExecutionContext.Implicits.global
    
    val ran = new AtomicBoolean(false)

    val f = LazyConditionalFuture[Int](
      () => Future {
        ran.set(true)
        5678
      },
      completeFirst = Future.successful(1234),
      ifCond = () => false
    )
    
    // shouldn't ever run the future that returns 5678
    f.futureValue shouldEqual 1234
    ran.get() shouldBe false
  }
  
  test("LazyConditionalFuture should run if condition is true, but only after the first future completes") {
    import scala.concurrent.ExecutionContext.Implicits.global

    val count = new AtomicInteger(-5)
    val p  = Promise[Unit]()

    // the order the code is run in is shown below in comments. We enforce this with compareAndSet.
    
    val f = LazyConditionalFuture(
      () => Future {
        count.compareAndSet(5, 10) // third
      },
      p.future,
      () => {
        count.compareAndSet(0, 5) // second
      }
    )
    
    p.completeWith(Future {
      count.compareAndSet(-5, 0) // first
    })
    
    f.futureValue shouldBe true
    count.get() shouldEqual 10
  }
  
  test("withTimeout", Slow) {
    val patience = PatienceConfig(timeout = Span(10, Seconds), interval = Span(50, Millis))

    implicit val system  = ActorSystem("FutureUtilsTest_withTimeout")
    implicit val scheduler = system.scheduler
    import system.dispatcher

    val neverComplete = Promise[Unit].future
    
    val sw = new Stopwatch()

    val exception = new Exception("timed out")
    val failed = neverComplete.withTimeout(FiniteDuration(2, "s"), exception)
    
    failed.failed.futureValue(patience, implicitly[source.Position]) shouldEqual exception
    sw.elapsedMs() should be > 1800l
    
    sw.restart()
    val success = Future.successful(1).withTimeout(FiniteDuration(2, "s"), exception)

    success.futureValue shouldEqual 1
    sw.elapsedMs() should be < 500l
  }
}
