package ru.primetalk.contacts.core

import ru.primetalk.contacts.dot.{BlocksShow, DotToBlocks, SystemDiagram, SystemDiagramToDotGraph}
import UniSets._

object SystemDiagramToDotGraph1 extends SystemDiagramToDotGraph with DotToBlocks with BlocksShow

trait BreadboardToDiagram extends ComponentAlgebraDependent { self =>

  import SystemDiagramToDotGraph1._
  trait DiagramNodeInfo[+C] { // we need covariance for Render[ DiagramNodeInfo [Contact
    def asDiagramNode: ComponentNode
  }
  class SimpleDiagramNodeInfo[+C](name: String) extends DiagramNodeInfo[C]{
    override def asDiagramNode: ComponentNode = ComponentNode(name, name)
  }
  sealed trait AsDiagram[B <: Breadboard] {
    def asDiagram: Diagram
  }
  implicit class AsDiagramOps[B <: Breadboard](b: B) {
    def asDiagram(implicit asDiagram: AsDiagram[B]): Diagram = asDiagram.asDiagram
  }
  implicit def emptyBreadboardToAsDiagram: AsDiagram[EmptyBreadboard] = new AsDiagram[EmptyBreadboard] {
    override def asDiagram: Diagram = Diagram("", Nil, Nil)
  }
  implicit def withAddedComponentBreadboardToAsDiagram[B0 <: Breadboard, C <: Component]
  (
    implicit
    cInfo: DiagramNodeInfo[C],
    d0: AsDiagram[B0],
    i1: Render[DiagramNodeInfo[Contact], Map[C#In,DiagramNodeInfo]],
    o1: Render[DiagramNodeInfo[Contact], Map[C#Out,DiagramNodeInfo]]
  ): AsDiagram[WithAddedComponent[C, B0]]  = new AsDiagram[WithAddedComponent[C, B0]] {
    override def asDiagram: Diagram = {
      val diagram = d0.asDiagram
      val inputs = i1.elements.toList.map(_.asDiagramNode)
      val outputs = o1.elements.toList.map(_.asDiagramNode)
      val component = cInfo.asDiagramNode
      if(inputs.size == 1 && outputs.size == 1){
        diagram.copy(
          nodes =
            inputs reverse_:::
            outputs reverse_:::
            diagram.nodes,
          links =
            inputs.flatMap(i =>
              outputs.map(o =>
                LinkInfo(LinkId(i, o), Some(component.name))
              )
            ) :::
              diagram.links
        )
      } else
        diagram.copy(
          nodes = component ::
            inputs reverse_:::
            outputs reverse_:::
            diagram.nodes,
          links =
              inputs.map(i => LinkInfo(LinkId(i, component), None)) :::
                outputs.map(o => LinkInfo(LinkId(component, o), None)) :::
                diagram.links
        )
    }
  }
}
