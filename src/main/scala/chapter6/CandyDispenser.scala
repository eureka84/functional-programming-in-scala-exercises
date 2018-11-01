package chapter6

import State._

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  private def update: Input => Machine => Machine = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) => Machine(locked = false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) => Machine(locked = true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val toStates: Input => State[Machine, Unit] = update andThen modify[Machine]
    for {
      _ <- sequence(inputs map toStates)
      s <- get
    } yield (s.coins, s.candies)
  }

//  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
//    State((m: Machine) => {
//      val machine = inputs.foldLeft(m)(handleInput)
//      ((machine.coins, machine.candies), machine)
//    })
//
//  private def handleInput(m: Machine, i: Input): Machine = i match {
//    case _ if m.candies == 0 => m
//    case Coin if m.locked => m.copy(locked = false, coins = m.coins + 1)
//    case Turn if !m.locked => m.copy(locked = true, candies = m.candies - 1)
//    case _ => m
//  }

}