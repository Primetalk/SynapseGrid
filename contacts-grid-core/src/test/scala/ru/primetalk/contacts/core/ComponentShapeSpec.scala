package ru.primetalk.contacts.core

import org.specs2.Specification

class ComponentShapeSpec extends Specification with ComponentShapeBuilderAPI { def is = s2"""

  This is specification of ComponentShape

    - inputs of the component should be known set $inputsEq
  """
  abstract class ContactImpl[T](val name: String) extends Product with Serializable
  override type Contact[T] = ContactImpl[T]
  case object In1 extends ContactImpl[String]("In1")
  case object Out1 extends ContactImpl[String]("Out1")
  val myComponent: ComponentShape {
    type InputShape = ConsTypeSet[In1.type, ∅]
    type OutputShape = ∅
  } = addInput[String, In1.type, EmptyComponentShape.type](In1, EmptyComponentShape)

  val inputs: ConsTypeSet[In1.type, Empty.type] = addElement(In1, ∅)
  def inputsEq = inputs must be(myComponent.inputs)
}
