package com.topaz.utils

import com.topaz.utils.IOUtils.IOThreadPool
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.tags.Slow
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

@Slow
class JvmLauncherTests extends AnyFunSuite with Matchers with EitherTestPimps with ScalaFutures {
  implicit val defaultPatience = PatienceConfig(timeout = Span(5, Seconds), interval = Span(50, Millis))

  test("fork") {
    val pool = new IOThreadPool(1, "testiopool")
    val proc = JvmLauncher.fork("com.topaz.utils.EchoMain", Nil, Seq("42"), inheritOutput = false).run(pool)
    proc.futureValue.waitFor() shouldEqual 42
  }
}

object EchoMain {
  def main(args: Array[String]) {
    System.exit(args(0).toInt)
  }
}
