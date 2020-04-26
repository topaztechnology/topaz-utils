package com.topaz.utils

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class StringUtilsTest extends AnyFreeSpec with Matchers {
  "isNullOrEmptyOrWhitespace" in {
    import StringUtils._

    isNullOrEmptyOrWhitespace(null) shouldBe true
    isNullOrEmptyOrWhitespace("") shouldBe true
    isNullOrEmptyOrWhitespace(" ") shouldBe true
    isNullOrEmptyOrWhitespace(" s") shouldBe false
  }

  "YAML to JSON conversion" in {
    val yaml =
      """
        |info:
        |  title: API
        |  version: 1
      """.stripMargin

    StringUtils.yamlToJson(yaml) shouldEqual """{"info":{"title":"API","version":1}}"""
  }
}
