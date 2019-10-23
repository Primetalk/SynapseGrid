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

  def parse(s: String): Iterable[Int] = Try(s.toInt).fold(_ => Iterable.empty, Iterable.single)
  def show(i: Int): String = i.toString

  val Parse: SignalOnContacts[In1.type +: core.TypeSets.∅] => Iterable[SignalOnContacts[Out1.type +: core.TypeSets.∅]] = liftIterable(In1, Out1)(parse)
  val Show: SignalOnContacts[In2.type +: primetalk.contacts.core.TypeSets.∅] => Iterable[SignalOnContacts[Out2.type +: primetalk.contacts.core.TypeSets.∅]] = lift(In2, Out2)(show)
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
