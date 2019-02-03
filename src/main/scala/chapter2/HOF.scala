package chapter2

object HOF {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    if (as.length < 2) true
    else
      as.take(as.length -1)
        .zip(as.drop(1))
        .forall({ case (a, b) => ordered(a, b) })

  def compose[A, B, C](f: B => C, g: A => B): A => C = g andThen f
}
