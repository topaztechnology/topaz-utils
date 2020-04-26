package com.topaz.utils

import java.util.Random
import org.apache.commons.math3.random.{MersenneTwister, RandomGenerator}
import org.scalatest.Suite
import org.scalatest.exceptions.TestFailedException


trait RandomTestSuite extends Log with RandomTestUtils {

  self: Suite =>

  private def withRandomGeneratorSingleTest[T](seed: Long) (block: RandomGenerator => T) = {
    self.withClue(s"Random seed was ${seed}l\n") {
      try {
        val rng = new MersenneTwister(seed)
        block(rng)
      } catch {
        case e: TestFailedException =>
          println(f"Seed was ${seed}l")
          throw e

        case e: Exception =>
          println(f"Seed was ${seed}l")
          throw new TestFailedException(e, 0)
      }
    }
  }

  def withRandomGenerator[T](seed: Long = -1, numTests: Int = 1) (block: RandomGenerator => T): T = {
    var result: T = null.asInstanceOf[T]

    if (seed != -1) {
      /* Failing tests will report the seed used - using with that seed will
       * just run a single test */
      result = withRandomGeneratorSingleTest(seed)(block)
    } else {
      val seedGenerator = new MersenneTwister((new Random()).nextLong())
      for (i <- 1 to numTests) {
        val seed_ = seedGenerator.nextLong().abs              /* Life hack - makes test failure seeds easier to copy/paste
                                                               * as double-clicking on a work to highlight it won't include
                                                               * a leading '-'
                                                               */
        result = withRandomGeneratorSingleTest(seed_)(block)

      }
    }
    result
  }
  private var twister: Option[MersenneTwister] = None

  private def getTwister() = synchronized[MersenneTwister] {
    twister match {
      case Some(t) => t
      case None => {
        val seed = new Random().nextLong()
        twister = Some(new MersenneTwister(seed))
        twister.get
      }
    }
  }

}
