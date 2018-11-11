package chapter7

import java.util.concurrent.{ExecutorService, Executors, TimeUnit}

import chapter7.Par.Par
import chapter7.Examples._
import org.scalatest._

class ParTest extends FunSuite with Matchers {

  private val es: ExecutorService = Executors.newCachedThreadPool()

  test("sum"){
    val parallelSum: Par[Int] = sum(Array(1, 2, 3, 4))
    val value = Par.run(es)(parallelSum)
    value shouldEqual 10
  }

  test("run function asynchronously"){
    val f: Int => Int = (n: Int) => n * n
    val fAsync: Int => Par[Int] = asyncF(f)
    Par.run(es)(fAsync(2)) shouldEqual 4
  }

  test("parMap") {
    val par: Par[List[Int]] = parMap(List(1, 2, 3))(a => a * a)

    Par.run(es)(par) shouldEqual List(1, 4, 9)
  }

  test("parFilter") {
    val par: Par[List[Int]] = parFilter(List(1, 2, 3, 4))(a => a % 2 == 0)

    Par.run(es)(par) shouldEqual List(2, 4)
  }


  ignore("throwing an exception in a parallel computation makes it never terminate") {
    val delayedValue: Int => Par[Int] =  asyncF(n => {
      throw new ArithmeticException("Puppa")
      n
    })

    Par.run(es)(delayedValue(1)) shouldEqual 1
  }

  test("choice"){
    val cond = Par.unit(true)
    val ifTrue = Par.unit(1)
    val ifFalse = Par.unit(2)

    Par.run(es)(Par.choice(cond)(ifTrue, ifFalse)) shouldEqual 1
  }

}
