package chapter6

import chapter6.RNG._
import org.scalatest.FunSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class RNGTest extends FunSuite with GeneratorDrivenPropertyChecks {

  test("non negative number") {
    forAll { seed: Long =>
      val (randomInt, _) = nonNegativeInt(SimpleRNG(seed))
      assert(randomInt >= 0)
      assert(randomInt <= Int.MaxValue)
    }
  }


  test("double") {
    forAll { seed: Long =>
      val (randomDouble, _)= double(SimpleRNG(seed))
      assert(randomDouble >= 0)
      assert(randomDouble < 1)
    }

    assert(double(FixedRng(Int.MaxValue))._1 < 1)
  }

  test("intDouble"){
    forAll { seed: Long =>
      val ((randomInt, randomDouble), _) = intDouble(SimpleRNG(seed))
      assert(randomInt >= 0)
      assert(randomInt <= Int.MaxValue)
      assert(randomDouble >= 0)
      assert(randomDouble < 1)
    }
  }

  test("doubleInt"){
    forAll { seed: Long =>
      val ((randomDouble, randomInt), _) = doubleInt(SimpleRNG(seed))
      assert(randomInt >= 0)
      assert(randomInt <= Int.MaxValue)
      assert(randomDouble >= 0)
      assert(randomDouble < 1)
    }
  }

  test("double3"){
    forAll { seed: Long =>
      val ((d1, d2, d3), _) = double3(SimpleRNG(seed))

      val doubles = List(d1, d2, d3)

      assert(doubles.forall(_ >=0))
      assert(doubles.forall(_ < 1))
    }
  }

}

case class FixedRng(value: Int) extends RNG {
  override def nextInt: (Int, RNG) = (value, this)
}
