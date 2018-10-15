package chapter2

import org.scalatest.FunSuite
import chapter2.Currying._

class CurryingTests extends FunSuite {

  test("curry") {
    val sum: (Int, Int) => Int = (a: Int, b: Int) => a + b
    val curriedAddition: Int => Int => Int = curry(sum)


    assert(curriedAddition(4)(3) == sum(4,3))
  }

  test("uncurry") {
    val curriedAddition: Int => Int => Int = (x: Int) => (y: Int) => x + y
    val sum: (Int, Int) => Int = uncurry(curriedAddition)

    assert(sum(3,4) == curriedAddition(3)(4))
  }
}
