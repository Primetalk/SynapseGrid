package ru.primetalk.contacts.core

import java.io.File

import org.specs2.Specification
import ru.primetalk.contacts.dot.ToIndentedLines
//import TypeSets._
import ru.primetalk.contacts.core.UniSets._

import scala.util.Try
import SystemDiagramToDotGraph1._
import ru.primetalk.contacts.dot.ToIndentedLines._
import ru.primetalk.contacts.dot.Show._
import ru.primetalk.contacts.dot.WritableStringDSL._

class Breadboard3Spec extends Specification
  with ComponentAlgebraDependent
  with MySignals
  with NamedContacts
  with BreadboardToDiagram
{ def is = s2"""

  This is specification of Breadboard

    - inputs of the component should be known set inputsEq
  """


//  type In1 = NamedContact["In1", String]
//  val In1: In1 = NamedContact["In1", String]
//
//  //  val in2 = contact["in2", Int]
////  val ev = implicitly[NamedContact["in1", Int] =:= NamedContact["in1", Int] ]
////  val ev = implicitly[in1.type =:= in2.type]
////  case object In1 extends ContactImpl[String]("In1")
//  case object Out1 extends ContactImpl[Int]("Out1")
//
//  case object In2 extends ContactImpl[Int]("In2")
//  case object Out2 extends ContactImpl[String]("Out2")

//  val out2 = valueOf[Out2.type]
//  val out1 = valueOf[Out1.type]
  object Parser extends InOutComponent[String, Int]
  object Shower extends InOutComponent[Int, String]
  object Incrementer extends LinkTwoInOutComponents[Parser.type, Shower.type] {
    val c1: Parser.type = Parser
    val c2: Shower.type = Shower
  }

  // Sink   = In1, In2, Out1
  // Source = Out1, Out2, In2
  // I      = In1
  // O      = Out2
  // source in Union(O, Sink)
  val both: ParallelAdd[Parser.type, Shower.type] =
    parallelAdd(Parser, Shower)


  val bbParser = EmptyBreadboard.withAddedComponent[Parser.type]
  type bbParserImpl = bbParser.Impl
  //  val bbParser = addComponentToBreadboard(emptyBreadboard, Parser)
  val bbParser_Shower = bbParser.withAddedComponent[Shower.type]
  type bbParser_ShowerImpl = bbParser_Shower.Impl
  val bbParserIncrementerShower = bbParser_Shower.withAddedComponent[Incrementer.type]
  type bbParserIncrementerShowerImpl = bbParserIncrementerShower.Impl
  val stringStringComponent = bbParserIncrementerShower.toComponent[Parser.In, Shower.Out]

  type stringStringComponentType = stringStringComponent.Self


  def parse(s: String): Iterable[Int] = Try(s.toInt).toOption
  implicit val parserImpl: HandlerOf[Parser.type] = defineComponentIterable(Parser)(parse)
//: HandlerOf[UniSets.Singleton[NamedContact["In1", String]], UniSets.Singleton[Out1.type], Parser.type]
  //  In1.map(s => Try(s.toInt).toOption)
  def show(i: Int): String = i.toString
  implicit val showerImpl = defineComponentImpl(Shower)(show)
//  implicit val showerImpl2 = //Shower.lift(show(_))
//    new ComponentOps[In2.type, Out2.type, Shower.type](Shower).lift(show)

  def inc(i: Int): Int = i + 1
  implicit val incrementerImpl = defineComponentImpl(Incrementer)(inc)


  val bothImpl = implicitly[HandlerOf[ParallelAdd[Parser.type, Shower.type]]]

  val EmptyBreadboardHandler = implicitly[HandlerOf[ImplementationComponent[ EmptyBreadboard]]]
  val parAddH = parallelAddHandlerOf[Parser.type, ImplementationComponent[EmptyBreadboard]]
  val BBParserHandler =  implicitly[HandlerOf[bbParser.Impl]]
  val BBParser_ShowerHandler =  implicitly[HandlerOf[bbParser_Shower.Impl]]

  val bbHandler = implicitly[HandlerOf[bbParserIncrementerShower.Impl]]

  val tco = toComponentHandlerOf[Parser.In, Shower.Out, bbParserIncrementerShower.Self]

  val stringStringImpl = implicitly[HandlerOf[stringStringComponentType]]//(tco)

//  (
//    toComponentHandlerOf[
//      bbParserIncrementerShower.Sinks0,
//      bbParserIncrementerShower.Sources0 ,
//      bbParserIncrementerShower.type,  Si[In1], Si[Out2.type]])

//  val both = parallelAddComponent(Parser, Shower)
//
//  def printer(s: String): Unit = println("printer: " + s)
//  val liftedPrinter: Si[In1] >> Empty = liftI1(In1)(printer)
//  val Printer = createComponent(shapePrinter)(liftedPrinter)
//
  val inputSignal1: SignalOnContact {
    type C = Parser.InContact
  } =  SignalOnContact.create[Parser.InContact]("10")
  val inputSignal =  signal[Parser.In](inputSignal1)
  val res = stringStringImpl.handler(inputSignal)

  res.flatMap(_.unwrap(Shower.outContact)).foreach { int =>
    println(int)
  }

  val diagramForEmptyBreadboard =
    implicitly[AsDiagram[EmptyBreadboard]].asDiagram
  implicit object ParserInfo extends ComponentNodeInfo[Parser.type]("Parser")
  implicit object ShowerInfo extends ComponentNodeInfo[Shower.type]("Shower")
  implicit object IncrementerInfo extends ComponentNodeInfo[Incrementer.type]("Incrementer")

//  implicit object ParserInInfo extends SimpleDiagramNodeInfo[Parser.InContact]("ParserIn")
//  implicit object ParserOutInfo extends SimpleDiagramNodeInfo[Parser.OutContact]("ParserOut")

  implicit def CompInInfo[C <: InOutComponent0](implicit cInfo: DiagramNodeInfo[C]): DiagramNodeInfo[C#InContact] =
    new NamedContactNodeInfo[C#InContact](cInfo.asDiagramNode.id+"_In")
  implicit def CompOutInfo[C <: InOutComponent0](implicit cInfo: DiagramNodeInfo[C]): DiagramNodeInfo[C#OutContact] =
    new NamedContactNodeInfo[C#OutContact](cInfo.asDiagramNode.id+"_Out")
  implicit def convertImplicitToValueOf[T](implicit t: T): ValueOf[T] = new ValueOf[T](t)
  val a1 = implicitly[Render[DiagramNodeInfo[Contact], Map[Parser.In,DiagramNodeInfo]]]
//  (
//    MapSingletonRender(
//      convertImplicitToValueOf(
//        CompInInfo[Parser.type](ParserInfo)),
//      ev = implicitly[<:<[DiagramNodeInfo[Parser.InContact], DiagramNodeInfo[Contact]]]
//    )
//  )


  val diagramForBbParser: Diagram =
    implicitly[AsDiagram[bbParser.Self]](withAddedComponentBreadboardToAsDiagram[EmptyBreadboard, Parser.type]).asDiagram
  val bbParserIncrementerShowerDiagram: Diagram =
    implicitly[AsDiagram[bbParserIncrementerShower.Self]].asDiagram
  val g1: graph = bbParserIncrementerShowerDiagram.toDotGraph
  val blockElement: BlockElement = g1.toBlocks
  val lines = ToIndentedLines.ToIndentedLinesOps(blockElement).toIndentedLines
  val str = lines.show
  str.saveTo("d2.dot")
  scala.sys.process.Process("dot -Tpng d2.dot").#>(new File("d2.png")).!
  //
}
