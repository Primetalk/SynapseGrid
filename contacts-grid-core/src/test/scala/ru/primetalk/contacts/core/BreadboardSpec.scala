package ru.primetalk.contacts.core

import org.specs2.Specification
//import TypeSets._
import UniSets._
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
//  val shape1: ComponentShape {
//    type InputShape = In1.type +: Empty
//    type OutputShape = Out1.type +: Empty
//  } = InOutShape(In1, Out1)
//  val shape2: ComponentShape {
//    type InputShape = In2.type +: Empty
//    type OutputShape = Out2.type +: Empty
//  } = InOutShape(In2, Out2)
//  val Parser = createComponent(shape1)(Parse)
//  val Shower = createComponent(shape2)(Show)
//  val shape = addOutput(Out2, addInput(In2, shape1))
//  val inputShapesUnion = implicitly[UnionHelper[Parser.shape.InputShape, Shower.shape.InputShape]]
//  val outputShapesUnion = implicitly[UnionHelper[Parser.shape.OutputShape, Shower.shape.OutputShape]]
//  val signalOnContactsOps = implicitly[SignalOnContactsOps[SignalOnContacts]]
//  val both: Component {
//    type Shape = ComponentShape {
//      type InputShape = inputShapesUnion.Out
//      type OutputShape = outputShapesUnion.Out
//    }
//  } = parallelAddComponent(Parser, Shower)(inputShapesUnion, outputShapesUnion, signalOnContactsOps)
//
//  val belongsToInputs1 = implicitly[BelongsTo[In1.type, In1.type +: Empty]]
//  val belongs1: BelongsTo[In1.type, inputShapesUnion.Out] =
//    belongsToA[Parser.shape.InputShape, Shower.shape.InputShape, inputShapesUnion.type, In1.type](inputShapesUnion, belongsToInputs1)

//  val in1Wrapper = In1.wrapper[both.Shape#InputShape](signalOnContactsOps, belongs1)


//  val inputSignal = in1Wrapper("10")
//  val res = both.handler(inputSignal)
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
