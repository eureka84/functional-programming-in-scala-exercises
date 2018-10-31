package chapter6

object State {

  type State[S, +A] = S => (A, S)

  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def flatMap[S, A, B](f: State[S, A])(g: A => State[S, B]): State[S, B] =
    s => {
      val (a, nextState) = f(s)
      g(a)(nextState)
    }

  def map[S, A, B](s: State[S, A])(f: A => B): State[S, B] =
    flatMap(s)(f.andThen((b: B) => unit[S, B](b)))

  def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(sa)(a => map(sb)(b => f(a, b)))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List[A]()))((sa, acc) => map2[S, A, List[A], List[A]](sa, acc)(_ :: _))

}
