package com.topaz.utils

import org.scalatest.tags.Slow

import scala.util.Random
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

@Slow
class GZipTest extends AnyFunSuite with Matchers {

  test("zipping and unzipping") {
    val rand = new Random()
    val str = rand.nextString(1024 * 1024)
    val compressed = GZip.gzip(str)
    GZip.gunzip(compressed) shouldEqual str
  }
}
