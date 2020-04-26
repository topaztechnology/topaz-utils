package com.topaz.utils

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CacheTest extends AnyFunSuite with Matchers {

  test("not reentrant") {
    for (cache <- Seq(Cache.createStaticCache("not reentrant"), Cache.createLimitedCache("not reentrant", 100))) {
      def rec(id: String, i: Int): Int = {
        cache.memoize[String, Integer](id) {
          i match {
            case n if n < 2 => rec(id, n + 1)
            case n => n + 1
          }
        }
      }
      
      // This seems like a weird thing to test but ConcurrentHashMap.computeIfAbsent is not reentrant and will live
      // lock if you call it recursively. We work around that but end up with a StackOverflowError instead.
      // Which is better than a live lock but not great.
      // ConcurrentHashMap.computeIfAbsent is what the caching library caffeine uses.
      // https://github.com/ben-manes/caffeine/issues/89
      // ConcurrentHashMap.computeIfAbsent is fixed in JDK9 (https://bugs.openjdk.java.net/browse/JDK-8062841)
      // which was released on 2017-09-21. We should revisit when we upgrade to it.
      intercept[StackOverflowError] {
        rec("blah", 0)
      }
    }
  }
  
  test("caching works") {
    for (cache <- Seq(Cache.createStaticCache("caching"), Cache.createLimitedCache("caching", 100))) {
      val calls = new AtomicInteger(0)

      def someFunc(): Integer = {
        calls.incrementAndGet()
        10
      }

      (1 to 10000).par.foreach {
        _ =>
          cache.memoize("key") {
            someFunc()
          }
      }

      calls.get() shouldEqual 1
    }
  }
}
