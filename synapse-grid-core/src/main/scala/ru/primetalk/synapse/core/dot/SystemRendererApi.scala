///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 * (2-clause BSD license) See LICENSE
 *
 * Created: 20.03.2013
 */
package ru.primetalk.synapse.core.dot

import ru.primetalk.synapse.core.components.{Contact0, InnerSystemComponent, StateUpdate, StateHandle0}
import ru.primetalk.synapse.core.ext.{AuxNumberingExt, ContactStyleExt, DevNullExt}

import scala.collection.mutable


/**
 * A customizable renderer of StaticSystem2 
 * @author А.Жижелев
 *
 */
trait SystemRendererApi extends ContactStyleExt with DevNullExt with AuxNumberingExt {

  trait SystemRenderer {

    sealed trait NodeKind

    case object InputNode extends NodeKind

    case object InnerContact extends NodeKind

    case object OutputNode extends NodeKind

    case object ComponentNode extends NodeKind

    case object StateNode extends NodeKind

    val stateLegacyModification = "shape=invhouse, fillcolor=lightpink, color=red, style=filled"
    val stateModification = "shape=octagon, color=red, style=rounded"

    def quote(m: String): String = "\""+m.replace("\"","\\\"")+"\""
    protected
    def stateNodeToDot(stylesExtOpt: Option[ContactStyleStaticExtension], id: Int, c: Any, nodeKind: NodeKind): String = (c, nodeKind) match {
      case (StateHandle(name, _), StateNode) =>
        s"$id [label=${quote(name)}, shape=tab, fillcolor=mistyrose, color=violetred, style=filled]"
      case (StateHandle(name, _), _) =>
        s"$id [label=${quote(name)+"-weird"}, shape=tab, fillcolor=mistyrose, color=violetred, style=filled]"
      case (_, _) =>
        throw new IllegalArgumentException(s"Cannot convert to Dot ${(c, nodeKind)}")
    }

    protected
    def linkNodeToDot(stylesExtOpt: Option[ContactStyleStaticExtension], id: Int, c: Link[_,_,_,_], nodeKind: NodeKind): String = c match {
      case Link(_, _, name, NopLink()) =>
        s"$id [label=${quote(if name=="" then "Δt" else name)}, shape=square]"
      case Link(_, _, name, _: StatefulFlatMapLink[_, _, _]) =>
        s"$id [label=${quote(name)}, $stateLegacyModification]"
      case Link(_, _, name, StateZipLink(_)) =>
        s"$id [label=${quote(name)}, shape=none]"
      case Link(_, c@Contact(_), name, linkInfo) if stylesExtOpt.isDefined && stylesExtOpt.get.style(c) == DevNullContact =>
        s"$id [label=${quote(name)}, shape=none, fontcolor=red]"
      case Link(_, _, name, linkInfo) =>
        s"$id [label=${quote(name)}, shape=none]"
    }

    protected
    def componentNodeToDot(stylesExtOpt: Option[ContactStyleStaticExtension], id: Int, c: Component, nodeKind: NodeKind): String = c match {
      case StateUpdate(_, st, name, _) =>
        s"$id [label=${quote(name)}, $stateModification]"
      case _ =>
        s"$id [label=${quote(c.name)}, shape=component]"
    }

    protected
    def contactNodeToDot(stylesExtOpt: Option[ContactStyleStaticExtension], id: Int, c: Any, nodeKind: NodeKind): String = (c, nodeKind) match {
      case (c@Contact(_), InnerContact) if stylesExtOpt.isDefined && stylesExtOpt.get.style(c) == DevNullContact =>
        s"$id [label=${quote("")}, shape=point, color=red]"
      case (c@Contact(_), InnerContact) if stylesExtOpt.isDefined && stylesExtOpt.get.style(c) == AuxiliaryContact =>
        s"$id [label=${quote("")}, shape=point]"
      case (Contact(name), InputNode) =>
        s"$id [label=${quote(name)}, shape=rectangle, style=${"\""}rounded,filled${"\""}, fillcolor=aquamarine]"
      case (Contact(name), InnerContact) =>
        s"$id [label=${quote(name)}, shape=ellipse]"
      case (Contact(name), OutputNode) =>
        s"$id [label=${quote(name)}, shape=rectangle, style=${"\""}rounded,filled${"\""}, fillcolor=cyan]"
      case (Contact(name), _) =>
        s"$id [label=${quote(name)}, shape=ellipse]"
      case _ =>
        throw new IllegalArgumentException(s"Cannot convert to dot ${(c, nodeKind)}")
    }

    //parallelogram
    def nodeToString(stylesExtOpt: Option[ContactStyleStaticExtension], id: Int, c: Any, nodeKind: NodeKind): String = (c, nodeKind) match {
      //			case (StateHandle(name, _), StateNode) =>
      //				s"$id [label=${"\"" + name + "\""}, shape=tab, fillcolor=mistyrose, color=violetred, style=filled]"
//      case (c: Link[_, _, _, _], _) => -- this version makes Scala3 compiler to fail
//        linkNodeToDot(stylesExtOpt, id, c, nodeKind)
      case (c@ Link(_, _, _, _), _) =>
        linkNodeToDot(stylesExtOpt, id, c, nodeKind)
      //			case (Link(_, _, _, NopLink()), _) =>
      //				s"$id [label=${"\"Δt\""}, shape=square]"
      //			case (Link(_, _, name, _: StatefulFlatMapLink[_, _, _]), _) =>
      //				s"$id [label=${"\"" + name + "\""}, $stateLegacyModification]"
      //			case (Link(_, _, name, StateZipLink(_)), _) =>
      //				s"$id [label=${"\"" + name + "\""}, shape=none]"
      //			case (Link(_, c@Contact(_), name, linkInfo), _) if stylesExtOpt.isDefined && stylesExtOpt.get.style(c) == DevNullContact =>
      //				s"$id [label=${"\"" + name + "\""}, shape=none, fontcolor=red]"
      //			case (Link(_, _, name, linkInfo), _) =>
      //				s"$id [label=${"\"" + name + "\""}, shape=none]"
      //			case (StateUpdate(_, st, name, _), _) =>
      //				s"$id [label=${"\"" + name + "\""}, $stateModification]"
      case (os: Component, _) =>
        componentNodeToDot(stylesExtOpt, id, os, nodeKind)
      //				s"$id [label=${"\"" + os.name + "\""}, shape=component]"
      case (c: Contact0, _) =>
        contactNodeToDot(stylesExtOpt, id, c, nodeKind)
      //			case (c@Contact(_), InnerContact) if stylesExtOpt.isDefined && stylesExtOpt.get.style(c) == DevNullContact =>
      //				s"$id [label=${"\"\""}, shape=point, color=red]"
      //			case (c@Contact(_), InnerContact) if stylesExtOpt.isDefined && stylesExtOpt.get.style(c) == AuxiliaryContact =>
      //				s"$id [label=${"\"\""}, shape=point]"
      //			case (Contact(name), InputNode) =>
      //				s"$id [label=${"\"" + name + "\""}, shape=rectangle, style=${"\""}rounded,filled${"\""}, fillcolor=aquamarine]"
      //			case (Contact(name), InnerContact) =>
      //				s"$id [label=${"\"" + name + "\""}, shape=ellipse]"
      //			case (Contact(name), OutputNode) =>
      //				s"$id [label=${"\"" + name + "\""}, shape=rectangle, style=${"\""}rounded,filled${"\""}, fillcolor=cyan]"
      case _ =>
        s"$id [label=${quote("unknown")}, shape=rectangle, style=${"\""}rounded,filled${"\""}, fillcolor=red]"
    }

    protected
    def linkToDot(idfrom: Int, idto: Int, from: Any, to: Any): String =
      s"$idfrom -> $idto "

    /** Customizable link from state. */
    protected
    def slinkToDot(idfrom: Int, idto: Int, from: Any, to: Any): String =
      s"$idfrom -> $idto [style=dashed, dir=none]"

    /** Update state link. */
    protected
    def suLinkToDot(idfrom: Int, idto: Int, from: Any, to: Any): String =
      s"$idfrom -> $idto [style=bold, dir=none]"

    class NodeCounter {
      private var _lastId = 0

      def next: Int = {
        _lastId += 1
        _lastId
      }

      def lastId: Int = _lastId
    }

    protected def newNodeCounter = new NodeCounter

//    def saveToDot(system: StaticSystem, filePath: String) {
//      val wrt = new PrintWriter(new File(filePath), "UTF-8")
//      try {
//        wrt.print(staticSystem2ToDot(system))
//      } finally {
//        wrt.close()
//      }
//    }
//
    def staticSystem2ToDot(system: StaticSystem,
                           graphKind: String = "digraph",
                           level: Int = 0,
                           counter: NodeCounter = newNodeCounter): String = {
      val stylesExtOpt = system.extensionOpt[ContactStyleStaticExtension]
      val elements = mutable.ListBuffer[String]()
      elements += "label=" + quote(system.name)
      elements += "rankdir = LR"
      if graphKind == "subgraph" then
        elements ++= List("fillcolor=azure", "style=filled")


      val ids = mutable.Map[Any, Int]()
      def getContactId(contact: Contact0, kind: NodeKind): Int = {
        if stylesExtOpt.isDefined && stylesExtOpt.get.style(contact) == DevNullContact then {
          val id = counter.next
          elements += nodeToString(stylesExtOpt, id, contact, kind)
          id
        } else
          ids.getOrElseUpdate(contact, {
            val id = counter.next
            elements += nodeToString(stylesExtOpt, id, contact, kind)
            id
          })
      }
      def getStateId(stateHandle: StateHandle0) =
        ids.getOrElseUpdate(stateHandle, {
          val id = counter.next
          elements += stateNodeToDot(stylesExtOpt, id, stateHandle, StateNode)
          id
        })
      def getComponentId(component: Component): Int = {
        //				case contact@Contact(_) if stylesExtOpt.isDefined && stylesExtOpt.get.style(contact) == DevNullContact =>
        //					elements += nodeToString(stylesExtOpt, counter.next, c, kind)
        //					counter.lastId
        //				case _ =>
        ids.getOrElseUpdate(component, {
          val id = counter.next
          elements += nodeToString(stylesExtOpt, id, component, ComponentNode)
          id
        })
      }
      val outputIds = for oc <- system.outputContacts
        yield getContactId(oc, OutputNode)
      val inputIds = for ic <- system.inputContacts
        yield getContactId(ic, InputNode)
      for s <- system.privateStateHandles do
        getStateId(s)

      elements += s"{rank=same; ${inputIds.mkString(" ")} }"
      elements += s"{rank=same; ${outputIds.mkString(" ")} }"
      for
        c <- system.components
      do {
        if level > 0 then
          c match {
            case comp: ComponentWithInternalStructure => //InnerSystem(s:StaticSystem, _, _) =>
              elements += staticSystem2ToDot(comp.toStaticSystem, "subgraph", level - 1, counter)
            case _ =>
          }
        val id = getComponentId(c)
        for i <- c.inputContacts do
          elements += linkToDot(getContactId(i, InnerContact), id, i, c)
        for o <- c.outputContacts do
          elements += linkToDot(id, getContactId(o, InnerContact), c, o)

        /** state link */
        c match {
          case Link(_, _, _, StateZipLink(st)) =>
            elements += slinkToDot(getStateId(st), id, st, c)
          case Link(_, _, _, StatefulFlatMapLink(_, st)) =>
            elements += slinkToDot(getStateId(st), id, st, c)
          case InnerSystemComponent(_, st, _) =>
            elements += slinkToDot(getStateId(st), id, st, c)
          case StateUpdate(_, st, _, _) =>
            elements += suLinkToDot(id, getStateId(st), c, st)

          case _ =>
        }
      }


      val graphId = if graphKind == "subgraph" then
        "cluster" + system.name
      else
        system.name
      val content = " {\n\t" + elements.mkString("\n\t") + "}"

      graphKind + " \"" + graphId + "\" " + content
    }
  }

  object SystemRenderer extends SystemRenderer with ((StaticSystem) => String) {
    def apply(s: StaticSystem): String = staticSystem2ToDot(s)

  }

}