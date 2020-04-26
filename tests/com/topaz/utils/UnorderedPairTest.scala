package com.topaz.utils

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class UnorderedPairTest extends AnyFunSuite with Matchers {

  test("works as expected") {
    val a = UnorderedPair(1, 2)
    val b = UnorderedPair(2, 1)
    a shouldEqual b
    a.hashCode() shouldEqual b.hashCode()
    
    // to string can't be the same without doing some ordering, which we don't do
    a.toString should not equal b.toString
    a.toString shouldEqual "UnorderedPair(1, 2)"
    b.toString shouldEqual "UnorderedPair(2, 1)"
    
    a.isDegenerate shouldBe false
    b.isDegenerate shouldBe false

    UnorderedPair(1, 1) shouldBe 'degenerate
  }
}
