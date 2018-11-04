package chapter7

import java.util.concurrent.{Executors, TimeUnit}

import chapter7.Par.Par
import chapter7.Examples._
import org.scalatest._

class ParTest extends FunSuite with Matchers {

  test("sum"){
    val parallelSum: Par[Int] = sum(Array(1, 2, 3, 4))
    val es = Executors.newFixedThreadPool(4)
    parallelSum(es).get(30, TimeUnit.SECONDS)  shouldEqual 10
  }

  test("run function asynchronously"){
    val es = Executors.newFixedThreadPool(1)
    val f: Int => Int = (n: Int) => n * n
    val fAsync: Int => Par[Int] = asyncF(f)
    fAsync(2)(es).get(5, TimeUnit.SECONDS) shouldEqual 4
  }

}
