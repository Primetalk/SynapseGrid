package ru.primetalk.contacts.core

import org.specs2.Specification
import org.specs2.matcher.MatchResult
import ru.primetalk
import ru.primetalk.contacts
import ru.primetalk.contacts.core
import ru.primetalk.contacts.core.TypeSets._

import scala.util.Try

class BreadboardSpec extends Specification with ComponentShapeBuilderAPI with MySignals { def is = s2"""

  This is specification of Breadboard

    - inputs of the component should be known set inputsEq
  """

  sealed trait MyContact extends Contact {
    type Data
  }

  abstract class ContactImpl[A](val name: String) extends Product with Serializable with MyContact {
    override type Data = A
  }

  case object In1 extends ContactImpl[String]("In1")
  case object Out1 extends ContactImpl[Int]("Out1")

  case object In2 extends ContactImpl[Int]("In2")
  case object Out2 extends ContactImpl[String]("Out2")

  def parse(s: String): Iterable[Int] = Try(s.toInt).toOption
  def show(i: Int): String = i.toString

  // : SignalOnContacts[In1.type +: core.TypeSets.∅] => Iterable[SignalOnContacts[Out1.type +: core.TypeSets.∅]]
  // : SignalOnContacts[In2.type +: ∅] => Iterable[SignalOnContacts[Out2.type +: ∅]]
  val Parse = liftIterable(In1, Out1)(parse)
  val Show  = lift(In2, Out2)(show)
  val shape1 = InOutShape(In1, Out1)
  val shape2 = InOutShape(In2, Out2)
  val Parser = createComponent(shape1)(Parse)
  val Shower = createComponent(shape2)(Show)
  val both = parallelAddComponent(Parser, Shower)
//
//  val myComponent: ComponentShape {
//    type InputShape = ConsTypeSet[In1.type, ∅]
//    type OutputShape = ∅
//  } = addInput[In1.type, EmptyComponentShape.type](In1, EmptyComponentShape)
//
//  val inputs: ConsTypeSet[In1.type, ∅] = addElement(In1, ∅)
//
//  def inputsEq: MatchResult[In1.type +: ∅] = inputs === myComponent.inputs
}
