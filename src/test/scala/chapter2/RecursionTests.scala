package chapter2

import org.scalatest.FunSuite
import Recursion.fib

class RecursionTests extends FunSuite {

  test("Fibonacci") {
    assert(fib(0) == 0)
    assert(fib(1) == 1)
    assert(fib(2) == 1)
    assert(fib(3) == 2)
    assert(fib(4) == 3)
    assert(fib(5) == 5)
    assert(fib(6) == 8)
  }
}
