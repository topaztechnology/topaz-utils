package com.topaz.utils

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class OSUtilsTest extends AnyFunSuite with Matchers {

  test("windows based auth works on windows") {
    if (OS.isWindows) {
      val (username, sid) = OSUtils.usernameAndDomain
      
      sid should not be empty
      sid.get should startWith("S-")
      username shouldNot be('empty)
    }
  }
}
