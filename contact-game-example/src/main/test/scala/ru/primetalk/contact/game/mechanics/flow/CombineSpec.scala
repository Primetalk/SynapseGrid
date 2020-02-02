package ru.primetalk.contact.game.mechanics.flow

import org.scalatest.{FlatSpec, Matchers}

class CombineSpec extends FlatSpec with Matchers with Combine{

  it should "be creatable" in {
    val component = new DelayedCombineComponent[String, String, String]()
    implicit val combineHandlerOf: HandlerOf[component.type] = defineCombineHandler[String, String, String, component.type](component)(_ + _ + "!")
    val leftSignal: SignalOnContact {
      type C = component.left.type
    } = SignalOnContact(component.left)("Hello")
    val s1 = signal[component.In](leftSignal)
    combineHandlerOf.handler(s1)
  }

}
