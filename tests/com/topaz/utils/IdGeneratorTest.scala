package com.topaz.utils

import java.util.concurrent.ConcurrentLinkedQueue

import org.scalatest.tags.Slow

import scala.collection.JavaConverters._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

@Slow
class IdGeneratorTest extends AnyFunSuite with Matchers {

  test("no overlap for different workers") {
    val ids = new ConcurrentLinkedQueue[GeneratedId]()
    (0 to 500).foreach {
      id =>
        val gen = new IdGenerator(id)
        (0 to 1000).par.foreach {
          _ =>
            ids.add(gen.nextId())
        }
    }
    ids.asScala.toList.distinct.size shouldEqual ids.size
  }
  
  test("wraps at max seq number") {
    (0 until 5).foreach {
      _ =>
        val workerId = 21
        val gen = new IdGenerator(workerId, IdGenerator.maxSequence - 1000)
        (0 to 1000).par.foreach {
          _ =>
            gen.nextId().id shouldBe >=(workerId.toLong)
        }
        gen.nextId().id shouldEqual workerId
    }
  }
}
