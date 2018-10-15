package chapter2

import org.scalatest.FunSuite
import chapter2.Currying._

class CurryingTests extends FunSuite {

  test("curry") {

    val curriedAddition: Int => Int => Int = curry((a: Int, b: Int) => a + b)

    val plusThree: Int => Int = curriedAddition(3)

    assert(plusThree(4) == 7)
  }

  test("uncurry") {
    def curriedAddition: Int => Int => Int = (x: Int) => (y: Int) => x + y

    def sum: (Int, Int) => Int = uncurry(curriedAddition)

    assert(sum(3,4) == 7)
  }
}
