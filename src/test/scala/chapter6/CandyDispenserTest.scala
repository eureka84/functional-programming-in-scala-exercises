package chapter6

import chapter6.Machine._
import org.scalatest.FunSuite

class CandyDispenserTest extends FunSuite {

  val LOCKED: Boolean = true
  val UNLOCKED: Boolean = false

  test("Inserting a coin into a locked machine will cause it to unlock if there’s any candy left") {
    val initialCandies = 1
    val initialCoins = 0

    val m = Machine(LOCKED, initialCandies, initialCoins)

    val ((coins, candies), newMachine) = simulateMachine(List(Coin)).run(m)

    assert(coins == initialCoins + 1)
    assert(candies == initialCandies)
    assert(newMachine == Machine(UNLOCKED, candies, coins))
  }

  test("Turning the knob on an unlocked machine will cause it to dispense candy and become locked.") {

    val initialCandies = 1
    val initialCoins = 1

    val m = Machine(UNLOCKED, initialCandies, initialCoins)

    val ((coins, candies), newMachine) = simulateMachine(List(Turn)).run(m)

    assert(coins == initialCoins)
    assert(candies == initialCandies - 1)
    assert(newMachine == Machine(LOCKED, candies, coins))

  }

  test("Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing") {

    val initialCandies = 1
    val initialCoins = 1

    val lockedMachine = Machine(LOCKED, initialCandies, initialCoins)
    val (_, afterTurn) = simulateMachine(List(Turn)).run(lockedMachine)
    assert(afterTurn == lockedMachine)

    val unlockedMachine = Machine(UNLOCKED, initialCandies, initialCoins)
    val (_, afterInserting) = simulateMachine(List(Coin)).run(unlockedMachine)
    assert(afterInserting == unlockedMachine)
  }

  test("A machine that’s out of candy ignores all inputs") {
    val initialCandies = 0
    val initialCoins = 1

    val m = Machine(UNLOCKED, initialCandies, initialCoins)

    val (_, nm) = simulateMachine(List(Turn, Coin, Coin, Turn, Turn)).run(m)

    assert(nm == m)
  }
}
