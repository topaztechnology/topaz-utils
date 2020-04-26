package com.topaz.utils

import org.apache.commons.math3.random.MersenneTwister
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class PimpedRandomGeneratorTests extends AnyFreeSpec with Matchers
  with RandomGeneratorPimps
{

  "partitioning works" in {

    val seeder = new MersenneTwister()
    for (_ <- 0 until 100) {
      val rng = new MersenneTwister(seeder.nextLong())
      val seq = 0 to (1 + rng.nextInt(20))
      val nPartitions = 1 + rng.nextInt(seq.size - 1)
      val partition = rng.partition(seq, nPartitions)
      partition.size shouldBe nPartitions
      partition.flatten shouldBe seq
      partition.foreach{
        p =>
          p shouldBe 'nonEmpty
      }

    }
  }
  "partitioning average sizes" in {
    val rng = new MersenneTwister(1234)

    val things = 1 to 20
    val nPartitions = 3
    val nRuns = 10000

    val partitionSizes = (1 to nRuns).map(_ => rng.partition(things, nPartitions).map(_.size))

    (0 until nPartitions).foreach {
      i =>
        val averageSize = partitionSizes.map(_(i)).sum / nRuns.toDouble
        averageSize shouldBe (things.size / nPartitions.toDouble +- 0.05)
    }


  }
}
