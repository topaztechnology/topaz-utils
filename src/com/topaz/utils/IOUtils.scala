package com.topaz.utils

import java.util.concurrent._

import scala.concurrent.{Future, _}
import scala.util.control.NonFatal

/**
  * This a thread pool for use in blocking IO operations
  *
  * See section 4.6 of below:
  * https://github.com/alexandru/scala-best-practices/blob/master/sections/4-concurrency-parallelism.md
  * Summed up by:
  * "if you're doing a lot of blocking I/O (e.g. a lot of calls to JDBC), it's better to create a 
  * second thread-pool / execution context and execute all blocking calls on that, leaving the 
  * application's thread-pool to deal with CPU-bound stuff."
  */
object IOUtils extends Log {

  class IOThreadPool(maxSize: Int, name: String) {
    private val ioThreadPool = {
      val factory = new NamedThreadFactory(s"$name(es-io-thread)", daemon = true, sequenced = true)
      // core pool size has to be the same as maxSize as the pool will never grow.
      // covered here with a work-around we may want to implement: 
      // https://github.com/kimchy/kimchy.github.com/blob/master/_posts/2008-11-23-juc-executorservice-gotcha.textile
      new ThreadPoolExecutor(
        maxSize /*core pool size */ , maxSize,
        60L, TimeUnit.SECONDS,
        new LinkedBlockingQueue[Runnable](),
        factory)
    }

    def executeBlockingIO[T](cb: => T): Future[T] = {
      val p = Promise[T]()

      ioThreadPool.execute(new Runnable {
        def run() = try {
          p.success(blocking(cb))
        } catch {
          case NonFatal(ex) =>
            log.error(s"Uncaught I/O exception", ex)
            p.failure(ex)
        }
      })

      p.future
    }

    def executeBlockingIO(r: Runnable): Unit = {
      ioThreadPool.execute(r)
    }

      /**
      * Only use for IO operations
      */
    val ioExecutionContext = ExecutionContext.fromExecutorService(ioThreadPool)
  }

  /**
    * For short run blocking operations. If you have something that is long running you should
    * create your own thread pool.
    */
  val global = new IOThreadPool(10, "GlobalIOThreadPool")
}
