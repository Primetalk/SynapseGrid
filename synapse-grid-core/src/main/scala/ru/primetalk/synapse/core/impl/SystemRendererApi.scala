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
package ru.primetalk.synapse.core.impl

import java.io.{File, PrintWriter}

import ru.primetalk.synapse.core._

import scala.collection.mutable


/**
 * A customizable renderer of StaticSystem2 
 * @author А.Жижелев
 *
 */
trait SystemRendererApi extends ContactStyleExt {

	trait SystemRenderer {

		val stateLegacyModification = "shape=invhouse, fillcolor=lightpink, color=red, style=filled"
		val stateModification = "shape=octagon, color=red, style=rounded"

		//parallelogram
		def nodeToString(stylesExtOpt: Option[ContactStyleStaticExtension], id: Int, c: Any, nodeKind: NodeKind): String = (c, nodeKind) match {
			case (StateHandle(name, _), StateNode) ⇒
				s"$id [label=${"\"" + name + "\""}, shape=tab, fillcolor=mistyrose, color=violetred, style=filled]"
			case (Link(_, _, _, NopLink()), _) ⇒
				s"$id [label=${"\"Δt\""}, shape=square]"
			case (Link(_, _, name, _: StatefulFlatMapLink[_, _, _]), _) ⇒
				s"$id [label=${"\"" + name + "\""}, $stateLegacyModification]"
			case (Link(_, _, name, StateZipLink(_)), _) ⇒
				s"$id [label=${"\"" + name + "\""}, shape=none]"
			case (Link(_, c@Contact(_), name, linkInfo), _) if stylesExtOpt.isDefined && stylesExtOpt.get.style(c) == DevNullContact ⇒
				s"$id [label=${"\"" + name + "\""}, shape=none, fontcolor=red]"
			case (Link(_, _, name, linkInfo), _) ⇒
				s"$id [label=${"\"" + name + "\""}, shape=none]"
			case (StateUpdate(_, st, name, _), _) ⇒
				s"$id [label=${"\"" + name + "\""}, $stateModification]"
			case (os: Component, _) ⇒
				s"$id [label=${"\"" + os.name + "\""}, shape=component]"
			case (c@Contact(_), InnerContact) if stylesExtOpt.isDefined && stylesExtOpt.get.style(c) == DevNullContact ⇒
				s"$id [label=${"\"\""}, shape=point, color=red]"
			case (c@Contact(_), InnerContact) if stylesExtOpt.isDefined && stylesExtOpt.get.style(c) == AuxiliaryContact ⇒
				s"$id [label=${"\"\""}, shape=point]"
			case (Contact(name), InputNode) ⇒
				s"$id [label=${"\"" + name + "\""}, shape=rectangle, style=${"\""}rounded,filled${"\""}, fillcolor=aquamarine]"
			case (Contact(name), InnerContact) ⇒
				s"$id [label=${"\"" + name + "\""}, shape=ellipse]"
			case (Contact(name), OutputNode) ⇒
				s"$id [label=${"\"" + name + "\""}, shape=rectangle, style=${"\""}rounded,filled${"\""}, fillcolor=cyan]"
		}

		def linkToString(idfrom: Int, idto: Int, from: Any, to: Any): String =
			s"$idfrom -> $idto "

		def slinkToString(idfrom: Int, idto: Int, from: Any, to: Any): String =
			s"$idfrom -> $idto [style=dashed, dir=none]"

		def suLinkToString(idfrom: Int, idto: Int, from: Any, to: Any): String =
			s"$idfrom -> $idto [style=bold, dir=none]"

		sealed trait NodeKind

		case object InputNode extends NodeKind

		case object InnerContact extends NodeKind

		case object OutputNode extends NodeKind

		case object ComponentNode extends NodeKind

		case object StateNode extends NodeKind

		class NodeCounter {
			private var _lastId = 0

			def next = {
				_lastId += 1; _lastId
			}

			def lastId = _lastId
		}

		def saveToDot(system: StaticSystem, filePath: String) {
			val wrt = new PrintWriter(new File(filePath), "UTF-8")
			try {
				wrt.print(staticSystem2ToDot(system))
			} finally {
				wrt.close()
			}
		}

		def staticSystem2ToDot(system: StaticSystem,
													 graphKind: String = "digraph",
													 level: Int = 0,
													 counter: NodeCounter = new NodeCounter): String = {
			val stylesExtOpt = system.extensionOpt[ContactStyleStaticExtension]
			val elements = mutable.ListBuffer[String]()
			elements += "label=\"" + system.name + "\""
			elements += "rankdir = LR"
			if (graphKind == "subgraph")
				elements ++= List("fillcolor=azure", "style=filled")


			val ids = mutable.Map[Any, Int]()
			def getId(c: Any, kind: NodeKind) = c match {
				case contact@Contact(_) if stylesExtOpt.isDefined && stylesExtOpt.get.style(contact) == DevNullContact ⇒
					elements += nodeToString(stylesExtOpt, counter.next, c, kind)
					counter.lastId
				case _ ⇒
					ids.getOrElseUpdate(c, {
						elements += nodeToString(stylesExtOpt, counter.next, c, kind)
						counter.lastId
					})
			}
			val outputIds = for (oc <- system.outputContacts)
				yield getId(oc, OutputNode)
			val inputIds = for (ic ← system.inputContacts)
				yield getId(ic, InputNode)
			for (s <- system.privateStateHandles)
				getId(s, StateNode)

			elements += s"{rank=same; ${inputIds.mkString(" ")} }"
			elements += s"{rank=same; ${outputIds.mkString(" ")} }"
			for {
				c ← system.components
			} {
				if (level > 0)
					c match {
						case comp: ComponentWithInternalStructure => //InnerSystem(s:StaticSystem, _, _) =>
							elements += staticSystem2ToDot(comp.toStaticSystem, "subgraph", level - 1, counter)
						case _ =>
					}
				val id = getId(c, ComponentNode)
				for (i ← c.inputContacts)
					elements += linkToString(getId(i, InnerContact), id, i, c)
				for (o ← c.outputContacts)
					elements += linkToString(id, getId(o, InnerContact), c, o)

				/** state link */
				c match {
					case Link(_, _, _, StateZipLink(st)) ⇒
						elements += slinkToString(getId(st, StateNode), id, st, c)
					case Link(_, _, _, StatefulFlatMapLink(_, st)) ⇒
						elements += slinkToString(getId(st, StateNode), id, st, c)
					case InnerSystemComponent(_, st, _) ⇒
						elements += slinkToString(getId(st, StateNode), id, st, c)
					case StateUpdate(_, st, _, _) ⇒
						elements += suLinkToString(id, getId(st, StateNode), c, st)

					case _ ⇒
				}
			}


			val graphId = if (graphKind == "subgraph")
				"cluster" + system.name
			else
				system.name
			val content = " {\n\t" + elements.mkString("\n\t") + "}"

			graphKind + " \"" + graphId + "\" " + content
		}
	}

	object SystemRenderer extends SystemRenderer with ((StaticSystem) => String) {
		def apply(s: StaticSystem) = staticSystem2ToDot(s)

	}

}