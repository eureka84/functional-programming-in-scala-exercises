package chapter6

object Rand2 {

  import State2._

  type Rand[A] = State2[RNG, A]

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    ra.map2(rb)((_, _))

  def sequenceRand[A](fs: List[Rand[A]]): Rand[List[A]] = sequence[RNG, A](fs)

  val int: Rand[Int] = State2(rng => rng.nextInt)

  def nonNegativeInt: Rand[Int] =
    int.map(i => if (i < 0) -(i + 1) else i)

  def nonNegativeEven: Rand[Int] =
    nonNegativeInt.map(i => i - i % 2)

  def double: Rand[Double] =
    nonNegativeInt.map(i => if (i == Int.MaxValue) 0.0 else i.toDouble / Int.MaxValue)

  def intDouble: Rand[(Int, Double)] =
    both(nonNegativeInt, double)

  def doubleInt: Rand[(Double, Int)] =
    both(double, nonNegativeInt)

  def double3: Rand[(Double, Double, Double)] =
    sequenceRand(List(double, double, double)).map {
      case d1::d2::d3::Nil => (d1, d2, d3)
    }

  def ints(count: Int): Rand[List[Int]] =
    sequenceRand(List.fill(count)(nonNegativeInt))

}
