package com.topaz.utils

import org.scalatest.tags.Slow
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

@Slow
class PIDTest extends AnyFunSuite with Matchers {

  test("pid and parent pid") {
    // this is really just testing we don't throw when looking for a PID
    PID() shouldBe > (0)
    PID() shouldBe < (1000000)
    
    PID.parent shouldBe > (0)
    PID.parent shouldBe < (1000000)
    
    PID.parent shouldNot equal (PID())
  }
}
