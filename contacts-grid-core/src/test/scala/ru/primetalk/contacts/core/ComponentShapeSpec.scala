package ru.primetalk.contacts.core

import org.specs2.Specification
import TypeSets._
import org.specs2.matcher.MatchResult

class ComponentShapeSpec extends Specification with ComponentShapeBuilderAPI { def is = s2"""

  This is specification of ComponentShape

    - inputs of the component should be known set $inputsEq
  """

  sealed trait MyContact extends Contact {
    type Data
  }

  abstract class ContactImpl[A](val name: String) extends Product with Serializable with MyContact {
    override type Data = A
  }

  case object In1 extends ContactImpl[String]("In1")
  case object Out1 extends ContactImpl[String]("Out1")

  val myComponent: ComponentShape {
    type InputShape = ConsTypeSet[In1.type, ∅]
    type OutputShape = ∅
  } = addInput[In1.type, EmptyComponentShape.type](In1, EmptyComponentShape)

  val inputs: ConsTypeSet[In1.type, ∅] = addElement(In1, ∅)

  def inputsEq: MatchResult[In1.type +: ∅] = inputs === myComponent.inputs
}
