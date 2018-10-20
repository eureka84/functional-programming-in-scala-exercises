package chapter4

import org.scalatest._

class OptionTest extends FunSuite with Matchers {

  test("map") {
    assert(Some(1).map(_.toString) == Some("1"))
    assert(None.map(_.toString) == None)
  }

  test("flatMap") {
    def inverse(x: Int): Option[Double] = if (x == 0) None else Some(1.0/x)
    assert(Some(1).flatMap(inverse) == Some(1.0))
    assert(Some(0).flatMap(inverse) == None)
  }

  test("getOrElse on Some"){
    Some(2).getOrElse(0) shouldEqual 2
  }

  test("getOrElse on None"){
    None.getOrElse(0) shouldEqual 0
  }

  test("orElse on Some"){
    Some(2).orElse(Some(0)) shouldEqual Some(2)
  }

  test("orElse on None"){
    None.orElse(Some(0)) shouldEqual Some(0)
  }

  test("filter on Some true"){
    Some(2).filter(_%2==0) shouldEqual Some(2)
  }

  test("filter on Some false") {
    Some(2).filter(_ % 3 == 0) shouldEqual None
  }

  test("filter on None"){
    None.orElse(Some(0)) shouldEqual Some(0)
  }

}
