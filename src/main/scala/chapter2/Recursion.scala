package chapter2

import scala.annotation.tailrec

/**
  * @author asciarra 
  */
object Recursion {

  def fib(n: Int): Int = {
    @tailrec
    def loop(counter: Int, last: Int, secondToLast: Int): Int =
      if (counter == n) last
      else loop(counter + 1, last + secondToLast, last)

    if (n <= 1) n
    else loop(1, 1, 0)
  }

}
