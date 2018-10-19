package chapter3

import org.scalatest.FunSuite
import Tree._
import IntTrees._

class TreeTests extends FunSuite {

  test("size of a Tree"){
    assert(size(Leaf(2)) == 1)
    assert(size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) == 5)
  }

  test("maximum") {
    assert(maximum(Leaf(5)) == 5)
    assert(maximum(
      Branch(
        Leaf(5),
        Branch(
          Leaf(2),
          Branch(
            Leaf(3),
            Leaf(4)
          )
        )
      )) == 5)
  }

  test("depth") {

    val tree =
      Branch(
        Leaf(5),
        Branch(
          Leaf(2),
          Branch(
            Leaf(3),
            Leaf(4)
          )
        )
      )

    println(tree)

    assert(depth(tree) == 4)
  }

}
