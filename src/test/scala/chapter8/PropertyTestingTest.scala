package chapter8

import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class PropertyTestingTest extends FunSuite with GeneratorDrivenPropertyChecks {

  test("sum"){
    forAll { xs: List[Int] =>
      xs.sum == xs.reverse.sum
    }


    val listSizes: Gen[Int] = Gen.choose(0, 100)
    val values: Gen[Int] = Gen.choose(Integer.MIN_VALUE, Integer.MAX_VALUE)


    forAll(values, listSizes) { (x: Int, n: Int) =>
      List.fill(n)(x).sum == n * x
    }
  }

}
