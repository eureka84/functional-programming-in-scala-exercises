package chapter6

import Rand2._
import org.scalatest.FunSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class Rand2Test extends FunSuite with GeneratorDrivenPropertyChecks {

  test("non negative number") {
    forAll { seed: Long =>
      val (randomInt, _) = nonNegativeInt.run(SimpleRNG(seed))
      assert(randomInt >= 0)
      assert(randomInt <= Int.MaxValue)
    }
  }


  test("double") {
    forAll { seed: Long =>
      val (randomDouble, _) = double.run(SimpleRNG(seed))
      assert(randomDouble >= 0)
      assert(randomDouble < 1)
    }

    assert(double.run(FixedRng(Int.MaxValue))._1 < 1)
  }

  test("intDouble") {
    forAll { seed: Long =>
      val ((randomInt, randomDouble), _) = intDouble.run(SimpleRNG(seed))
      assert(randomInt >= 0)
      assert(randomInt <= Int.MaxValue)
      assert(randomDouble >= 0)
      assert(randomDouble < 1)
    }
  }

  test("doubleInt") {
    forAll { seed: Long =>
      val ((randomDouble, randomInt), _) = doubleInt.run(SimpleRNG(seed))
      assert(randomInt >= 0)
      assert(randomInt <= Int.MaxValue)
      assert(randomDouble >= 0)
      assert(randomDouble < 1)
    }
  }

  test("double3") {
    forAll { seed: Long =>
      val ((d1, d2, d3), _) = double3.run(SimpleRNG(seed))

      val doubles = List(d1, d2, d3)

      assert(doubles.forall(_ >= 0))
      assert(doubles.forall(_ < 1))
    }
  }

  test("ints") {
    forAll { seed: Long =>
      val list = ints(10).run(SimpleRNG(seed))._1

      assert(list.length == 10 )
      val pairsOfSuccessive = list.take(9).zip(list.drop(1))
      assert(pairsOfSuccessive.forall {case (prev, next) => prev != next})
    }
  }

}
