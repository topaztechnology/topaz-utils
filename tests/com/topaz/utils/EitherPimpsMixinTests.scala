package com.topaz.utils

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class EitherPimpsMixinTests extends AnyFreeSpec with Matchers with EitherPimpsMixin {

  "fold left or error" in {
    foldOrErrorLeft(0, Seq(1, 2, 3))((x, y) => Right(x + y)) should be (Right(6))

    foldOrErrorLeft(0, Seq(1, 2, 3))(
      (x, y) => 
        if (y == 2)
          Left("Oops")
        else
          Right(x + y)
    ) should be ('left)
  }
}
