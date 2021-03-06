package chapter4

import org.scalatest._


class EitherTest extends FunSuite with Matchers {

  def inverse(x: Double): Either[String, Double] =
    if (x == 0) Left("Division by zero")
    else Right(1.0 / x)

  test("map") {
    val square: Int => Int = x => x * x
    Right(2) map square shouldEqual Right(4)
    Left("Already failed") map square shouldEqual Left("Already failed")
  }

  test("flatMap") {
    Right(2.0) flatMap inverse shouldEqual Right(0.5)
    Right(0.0) flatMap inverse shouldEqual Left("Division by zero")
  }

  test("orElse") {
    Right(2) orElse Right(3) shouldEqual Right(2)
    Left("Some error") orElse Right(3) shouldEqual Right(3)
  }

  test("map2") {
    def sumEither(a: Either[String, Double], b: Either[String, Double]): Either[String, Double] =
      a.map2(b)(_ + _)

    sumEither(inverse(2), inverse(2)) shouldEqual Right(1.0)
    sumEither(inverse(0), inverse(2)) shouldEqual Left("Division by zero")
  }

  test("sequence") {
    import  Either._

    sequence(List(Right(1), Right(2))) shouldEqual Right(List(1, 2))
    sequence(List(Right(1), Left("Error 1"), Left("Error 2"))) shouldEqual Left("Error 1")
  }

  test("traverse") {
    import Either._

    traverse(List(1.0, 2.0, 4.0))(inverse) shouldEqual Right(List(1.0, 0.5, 0.25))
    traverse(List(1.0, 0.0, 4.0))(inverse) shouldEqual Left("Division by zero")
  }

}
