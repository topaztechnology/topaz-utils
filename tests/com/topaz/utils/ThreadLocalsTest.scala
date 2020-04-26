package com.topaz.utils

import java.util.concurrent.atomic.AtomicBoolean

import org.scalatest.concurrent.Eventually
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ThreadLocalsTest extends AnyFunSuite with Matchers with Eventually {

  test("thread locals") {
    val key = "test"

    ThreadLocals.value(key) shouldBe 'empty

    ThreadLocals.withThreadLocal(key, 10) {
      ThreadLocals.value(key) shouldEqual Some(10)

      val ran = new AtomicBoolean(false)
      val foundValue = new AtomicBoolean(false)
      new Thread(() => {
        val value = ThreadLocals.value(key)
        foundValue.set(value.nonEmpty)
        ran.set(true)
      }).start()

      eventually {
        ran.get() shouldBe true
      }
      foundValue.get shouldBe false
    }

    ThreadLocals.value(key) shouldBe 'empty
  }
}
