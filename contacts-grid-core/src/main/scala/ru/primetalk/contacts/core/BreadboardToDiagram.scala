package ru.primetalk.contacts.core

import ru.primetalk.contacts.dot.{BlocksShow, DotToBlocks, SystemDiagram, SystemDiagramToDotGraph}
import UniSets._

object SystemDiagramToDotGraph1 extends SystemDiagramToDotGraph with DotToBlocks with BlocksShow

trait BreadboardToDiagram extends ComponentAlgebraDependent {
  self =>

  import SystemDiagramToDotGraph1._

  trait DiagramNodeInfo[+C] { // we need covariance for Render[ DiagramNodeInfo [Contact
    def asDiagramNode: DiagramNode
  }

  class ComponentNodeInfo[+C](name: String) extends DiagramNodeInfo[C] {
    override def asDiagramNode: ComponentNode = ComponentNode(name, name)
  }

  class NamedContactNodeInfo[+C](name: String) extends DiagramNodeInfo[C] {
    override def asDiagramNode: DiagramNode = NamedContactNode(name, name)
  }

  class AnonymousContactNodeInfo[+C](id: String) extends DiagramNodeInfo[C] {
    override def asDiagramNode: DiagramNode = AnonymousContactNode(id)
  }

  type ContactMap = Map[Contact, DiagramNode]

  sealed trait AsDiagram[B <: Breadboard] {
    def asDiagram: (Diagram, ContactMap)
  }

  implicit class AsDiagramOps[B <: Breadboard](b: B) {
    def asDiagram(implicit asDiagram: AsDiagram[B]): (Diagram, ContactMap) = asDiagram.asDiagram
  }

  implicit def emptyBreadboardToAsDiagram: AsDiagram[EmptyBreadboard] = new AsDiagram[EmptyBreadboard] {
    override def asDiagram: (Diagram, ContactMap) = (Diagram("breadboard", Nil, Nil), Map[Contact, ContactNode]())
  }

  type DiagramNodeInfoWithContact[+C] = (C, DiagramNodeInfo[C])
  implicit def diagramNodeInfoWithContact[C<: Contact: ValueOf](implicit dni: DiagramNodeInfo[C]): ValueOf[DiagramNodeInfoWithContact[C]] =
    new ValueOf((valueOf[C], dni))

  implicit def withAddedComponentBreadboardToAsDiagram[B0 <: Breadboard, C <: Component]
  (
    implicit
    cInfo: ComponentNodeInfo[C],
    d0: AsDiagram[B0],
//    i0: Render[DiagramNodeInfo[Contact], UniMap[Subtract[C#In, B0#Sources], DiagramNodeInfo]],
//    i1: Render[Contact, C#In],
    i1: Render[DiagramNodeInfoWithContact[Contact], UniMap[C#In, DiagramNodeInfoWithContact]],
//    i1: Render[DiagramNodeInfoWithContact[Contact], UniMap[C#In, DiagramNodeInfoWithContact]],
//    ei: Render[DiagramNodeInfo[Contact], UniMap[Intersection[C#In, B0#Sources], DiagramNodeInfo]],
//    o0: Render[DiagramNodeInfo[Contact], UniMap[Subtract[C#Out, B0#Sinks], DiagramNodeInfo]],
//    o1: Render[Contact, C#Out],
    o1: Render[DiagramNodeInfoWithContact[Contact], UniMap[C#Out, DiagramNodeInfoWithContact]],
//    o1: Render[DiagramNodeInfoWithContact[Contact], UniMap[C#Out, DiagramNodeInfoWithContact]],
//    eo: Render[DiagramNodeInfo[Contact], UniMap[Intersection[C#Out, B0#Sinks], DiagramNodeInfo]]
  ): AsDiagram[WithAddedComponent[C, B0]] = new AsDiagram[WithAddedComponent[C, B0]] {
    override def asDiagram: (Diagram, ContactMap) = {
      val (diagram, contactMap) = d0.asDiagram
      val knownContacts = contactMap.keySet
      val inputs = i1.elements.toList.map{ case (c, ni) => (c, ni.asDiagramNode) }
      val outputs = o1.elements.toList.map{ case (c, ni) => (c, ni.asDiagramNode) }
      val newInputs = inputs.filterNot(p => knownContacts.contains(p._1))
      val newOutputs = outputs.filterNot(p => knownContacts.contains(p._1))
      val component = cInfo.asDiagramNode
      val contactMap2: Map[Contact, DiagramNode] = contactMap ++ newInputs ++ newOutputs
      if (inputs.size == 1 && outputs.size == 1) {
        (diagram.copy(
          nodes =
            newInputs.map(_._2) reverse_:::
              newOutputs.map(_._2) reverse_:::
              diagram.nodes,
          links =
            inputs.flatMap(i =>
              outputs.map(o =>
                LinkInfo(LinkId(contactMap2(i._1), contactMap2(o._1)), Some(component.name))
              )
            ) :::
              diagram.links
        ), contactMap2)
      } else
        (diagram.copy(
          nodes = component ::
            newInputs.map(_._2) reverse_:::
            newOutputs.map(_._2) reverse_:::
            diagram.nodes,
          links =
            inputs.map(i => LinkInfo(LinkId(contactMap2(i._1), component), None)) :::
              outputs.map(o => LinkInfo(LinkId(component, contactMap2(o._1)), None)) :::
              diagram.links
        ), contactMap2)
//      ???
    }
  }
}
trait BreadboardToDiagramAutoNumberedContacts extends BreadboardToDiagram {
  private var anonymousContactId: Int = 0
  // fallback automatic information about node if nothing is available in the context
  implicit def contactInfo[C <: Contact]: DiagramNodeInfo[C] = {
    anonymousContactId += 1
    new AnonymousContactNodeInfo[C](anonymousContactId.toString)
  }

}
