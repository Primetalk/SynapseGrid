package ru.primetalk.contacts.core

import org.specs2.Specification
//import TypeSets._
import UniSets._
import scala.util.Try

class BreadboardSpec extends Specification with ComponentShapeBuilderAPI with MySignals { def is = s2"""

  This is specification of Breadboard

    - inputs of the component should be known set inputsEq
  """

  sealed trait MyContact extends Contact

  abstract class ContactImpl[A](val name: String) extends Product with Serializable with MyContact {
    override type Data = A
  }

  case object In1 extends ContactImpl[String]("In1")
  case object Out1 extends ContactImpl[Int]("Out1")

  case object In2 extends ContactImpl[Int]("In2")
  case object Out2 extends ContactImpl[String]("Out2")

  def parse(s: String): Iterable[Int] = Try(s.toInt).toOption
  def show(i: Int): String = i.toString

  def inc(i: Int): Int = i + 1

  // : SignalOnContacts[In1.type +: core.TypeSets.∅] => Iterable[SignalOnContacts[Out1.type +: core.TypeSets.∅]]
  // : SignalOnContacts[In2.type +: ∅] => Iterable[SignalOnContacts[Out2.type +: ∅]]
  val Parse: Singleton[In1.type] >> Singleton[Out1.type] = liftIterable(In1, Out1)(parse)
  val Show: Singleton[In2.type] >> Singleton[Out2.type] = lift(In2, Out2)(show)
  val shape1 = InOutShape[In1.type, Out1.type](In1, Out1)
  val shape2 = InOutShape[In2.type, Out2.type](In2, Out2)
  val Parser = createComponent(shape1)(Parse)
  val Shower = createComponent(shape2)(Show)
  val both = parallelAddComponent(Parser, Shower)

  def printer(s: String): Unit = println("printer: " + s)
  val shapePrinter: ComponentShape {
    type InputShape = Singleton[In1.type]
    type OutputShape = ∅
  } = componentShapeConverter(addInput(In1, EmptyComponentShape))
  val liftedPrinter: Si[In1.type] >> Empty = liftI1(In1)(printer)
  val Printer = createComponent(shapePrinter)(liftedPrinter)

  val inputSignal1: SignalOnContact {
    type C = In1.type
  } =  SignalOnContact(In1)("10")
  val inputSignal =  signal[both.shape.InputShape](inputSignal1)
  val res = both.handler(inputSignal)

  res.flatMap(_.unwrap(Out1)).foreach { int =>
    println(int)
  }

  val threeComponents = parallelAddComponent(both, Printer)

  val res3 = threeComponents.handler(signal[threeComponents.shape.InputShape](inputSignal1))

  res3.flatMap(_.unwrap(Out1)).foreach { int =>
    println("Output: " + int)
  }
  val bb1 = addComponentToBreadboard(emptyBreadboard, Printer)

  // composing In1 -> parse -> Out1 -> inc ->In2 -> show -> Out2 into a single component String->String

  val bbParser = addComponentToBreadboard(emptyBreadboard, Parser)
  val bbParser_Show = addComponentToBreadboard(bbParser, Shower)
  val Incrementer = createComponent(InOutShape[Out1.type, In2.type](Out1, In2))(lift(Out1, In2)(inc))
  val bbParserIncShow: Breadboard[BreadboardShape {
    type SourceShape = Union[Union[Union[Empty, Singleton[Out1.type]], Singleton[Out2.type]], Singleton[In2.type]]

    type SinkShape = Union[Union[Union[Empty, Singleton[In1.type]], Singleton[In2.type]], Singleton[Out1.type]]
  }] = addComponentToBreadboard(bbParser_Show, Incrementer)

//  val ev = implicitly[
//      Union[Singleton[Out1.type], Singleton[In2.type]]
//     IsSubSetOf
//      Union[Union[Singleton[In1.type], Singleton[In2.type]], Singleton[Out1.type]]]
//val ev = implicitly[
//    Union[Union[Empty, Singleton[Out1.type]], Singleton[In2.type]]
//    IsSubSetOf
//    Union[Union[Union[Empty, Singleton[In1.type]], Singleton[In2.type]], Singleton[Out1.type]]]
//  val ev = implicitly[
//    Subtract[
//      Union[Union[Union[Empty, Singleton[Out1.type]], Singleton[Out2.type]], Singleton[In2.type]],
//      Singleton[Out2.type]
//    ] IsSubSetOf
//      Union[Union[Union[Empty, Singleton[In1.type]], Singleton[In2.type]], Singleton[Out1.type]]]

//    bbParserIncShow.toComponent[Si[In1.type], Si[Out2.type]]
  val componentStringString: Component[ComponentShape {
    type InputShape = Si[In1.type]
    type OutputShape = Si[Out2.type]
  }] = bbParserIncShow.toComponent[Si[In1.type], Si[Out2.type]]

  val res11 = componentStringString.handler(signal[componentStringString.shape.InputShape](inputSignal1))
  res11.flatMap(_.unwrap(Out2)).foreach { str =>
    println("Incremented number: " + str)
  }
}
