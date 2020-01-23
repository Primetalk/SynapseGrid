package ru.primetalk.contacts.core

import java.io.File

import org.specs2.Specification
import ru.primetalk.contacts.dot.{IndentedLine, ToIndentedLines}
import ru.primetalk.contacts.core.UniSets._

import scala.util.Try
import SystemDiagramToDotGraph1._
import ru.primetalk.contacts.dot.ToIndentedLines._
import ru.primetalk.contacts.dot.Show._
import ru.primetalk.contacts.dot.WritableStringDSL._


class SimpleBreadboardDiagramSpec extends Specification
  with ComponentAlgebraDependent
  with MySignals
  with NamedContacts
  with BreadboardToDiagram
  with BreadboardToDiagramAutoNumberedContacts
{ def is = s2"""

  This is specification of a simple breadboard

    - inputs of the component should be known set inputsEq
  """

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
  def show(i: Int): String = i.toString
  implicit val showerImpl = defineComponentImpl(Shower)(show)

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
  implicit case object ParserInfo extends ComponentNodeInfo[Parser.type]("Parser")
  implicit case object ShowerInfo extends ComponentNodeInfo[Shower.type]("Shower")
  implicit case object IncrementerInfo extends ComponentNodeInfo[Incrementer.type]("Incrementer")

  val i1 = implicitly[Render[DiagramNodeInfoWithContact[Contact], UniMap[Parser.In, DiagramNodeInfoWithContact]]](MapSingletonRender)

  val (diagramForBbParser: Diagram, _) =
    implicitly[AsDiagram[bbParser.Self]](withAddedComponentBreadboardToAsDiagram[EmptyBreadboard, Parser.type]).asDiagram
  val (bbParserIncrementerShowerDiagram: Diagram, _) =
    implicitly[AsDiagram[bbParserIncrementerShower.Self]].asDiagram
  val g1: graph = bbParserIncrementerShowerDiagram.toDotGraph
  val blockElement: BlockElement = g1.toBlocks
  val lines: List[IndentedLine] = blockElement.toIndentedLines
  val str: String = lines.show
  str.saveTo("d2.dot")
  scala.sys.process.Process("dot -Tpng d2.dot").#>(new File("d2.png")).!
  //
}
