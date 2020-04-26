package com.topaz.utils

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CaseInsensitiveMapTest extends AnyFunSuite with Matchers {

  test("map is case insensitive") {
    var m: Map[String, Int] = CaseInsensitiveMap("a" -> 1, "B" -> 2)

    m.get("A") shouldBe Some(1)
    m.get("b") shouldBe Some(2)
    m.get("c") shouldBe None

    m = m + ("C" -> 3)
    m.get("c") shouldBe Some(3)

    m = m - "a"
    m.get("A") shouldBe None
    m.get("a") shouldBe None

    val normalMap = m.asInstanceOf[CaseInsensitiveMap[Int]].asNormalMap

    normalMap.get("B") shouldBe Some(2)
    normalMap.get("b") shouldBe None

    m.toSeq shouldEqual normalMap.toSeq
    m.toSeq should contain theSameElementsAs Seq(
      "B" -> 2,
      "C" -> 3,
    )
  }

  test("fail for invalid maps") {
    intercept[Exception] {
      CaseInsensitiveMap("a" -> 1, "A" -> 2)
    }.getMessage should include ("Can't create a CaseInsensitiveMap when multiple keys would map to the same value: a, A")
  }
}
