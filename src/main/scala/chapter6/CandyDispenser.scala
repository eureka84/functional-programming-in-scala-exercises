package chapter6

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  def simulateMachine(inputs: List[Input]): State2[Machine, (Int, Int)] =
    State2((m: Machine) => {
      val machine = inputs.foldLeft(m)(handleInput)
      ((machine.coins, machine.candies), machine)
    })

  private def handleInput(m: Machine, i: Input): Machine = i match {
    case _ if m.candies == 0 => m
    case Coin if m.locked && m.candies > 0 => m.copy(locked = false, coins = m.coins + 1)
    case Turn if !m.locked => m.copy(locked = true, candies = m.candies - 1)
    case _ => m
  }

}