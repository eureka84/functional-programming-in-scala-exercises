package chapter2

import org.scalatest.FunSuite
import chapter2.Currying._
/**
  * @author asciarra 
  */
class CurryingTests extends FunSuite {

  test("curry") {

    val curriedAddition: Int => Int => Int = curry((a: Int, b: Int) => a + b)

    val plusThree: Int => Int = curriedAddition(3)

    assert(plusThree(4) == 7)
  }

}
