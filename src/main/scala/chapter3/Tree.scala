package chapter3

sealed trait Tree[+A] {
  override def toString: String = this match {
    case Leaf(value) => s"($value)"
    case Branch(left, right) => s"[$left, $right]"
  }
}
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

}

object IntTrees {

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

}
