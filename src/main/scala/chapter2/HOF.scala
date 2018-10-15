package chapter2

/**
  * @author asciarra 
  */
object HOF {

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean =
    if (as.length < 2) true
    else
      (0 until as.length - 1)
        .zip(1 until as.length)
        .forall(pair => ordered(as(pair._1), as(pair._2)))


}
