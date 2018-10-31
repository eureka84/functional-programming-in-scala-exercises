package chapter6

import State2._

case class State2[S, +A](run: S => (A, S)) {
  def flatMap[B](g: A => State2[S, B]): State2[S, B] =
    State2((s: S) => {
      val (a, nextState) = run(s)
      g(a).run(nextState)
    })

  def map[B](f: A => B): State2[S, B] = {
    flatMap(f.andThen(unit[S, B]))
  }

  def map2[B, C](sb: State2[S, B])(f: (A, B) => C): State2[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
}

object State2 {
  def unit[S, A](a: A): State2[S, A] = State2((s: S) => (a, s))

  def sequence[S, A](fs: List[State2[S, A]]): State2[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List[A]()))((sa, acc) => sa.map2(acc)(_ :: _))
}