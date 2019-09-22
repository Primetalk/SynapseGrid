package ru.primetalk.synapse.core

import org.scalatest.{FlatSpecLike, Matchers}

class BlackBoxSwitcherSpec extends FlatSpecLike with Matchers {

  it should "filter even numbers" in {
    val even = input(2)
    val expected = BlackBoxSwitcher.evenOutput.signal(2)
    handler.receive(even).headOption shouldBe Some(expected)
  }

  it should "filter odd numbers" in {
    val odd = input(5)
    val expected = BlackBoxSwitcher.oddOutput.signal(5)
    handler.receive(odd).headOption shouldBe Some(expected)
  }

  private def handler = BlackBoxSwitcher.toDynamicSystem

  private def input(value: Int) = BlackBoxSwitcher.intInput.signal(value)

  object BlackBoxSwitcher extends BaseTypedSystem {
    val intInput: Contact[Int] = input[Int]("switcherInput")
    val evenOutput: Contact[Int] = output[Int]("even")
    val oddOutput: Contact[Int] = output[Int]("odd")

    override protected def defineSystem(implicit sb: SystemBuilder): Unit = {
      intInput.map {
        case n if n % 2 == 0 => Signal(evenOutput, n)
        case other => Signal(oddOutput, other)
      }.switcher("switch").fanOut(evenOutput, oddOutput)
    }
  }
}
