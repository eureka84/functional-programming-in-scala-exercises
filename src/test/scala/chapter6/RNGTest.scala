package chapter6

import chapter6.RNG._
import org.scalatest.FunSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class RNGTest extends FunSuite with GeneratorDrivenPropertyChecks {

  test("non negative number") {
    forAll { seed: Long =>
      assert(nonNegativeInt(SimpleRNG(seed))._1 >= 0)
      assert(nonNegativeInt(SimpleRNG(seed))._1 <= Int.MaxValue)
    }
  }

}
