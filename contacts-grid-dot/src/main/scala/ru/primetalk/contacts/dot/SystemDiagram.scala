package ru.primetalk.contacts.dot

/** This is an intermediate representation of a system's structure.
  * We capture
  * - components,
  * - component ports (input/output contacts),
  * - intermediate contacts,
  * - wires (links)
  * - subsystems.
  *
  * For many parts we capture names and styles.
  *
  * In order to identify nodes in the graph, we associate a unique identifier with each element.
  */
trait SystemDiagram {

  sealed trait DiagramNode {
    val id: String
  }
  sealed trait ContactNode extends DiagramNode
  case class NamedContactNode(id: String, name: String) extends ContactNode
  case class AnonymousContactNode(id: String) extends ContactNode

  /** Identifier of a link between two nodes. */
  case class LinkId(id1: DiagramNode, id2: DiagramNode)
  case class LinkInfo(linkId: LinkId, name: Option[String] = None)

  // , inputs: List[Contact], outputs: List[Contact]
  case class ComponentNode(id: String, name: String) extends DiagramNode

  case class Diagram(name: String, nodes: List[DiagramNode], links: List[LinkInfo])
}

trait SystemDiagramToDotGraph extends SystemDiagram with DotAST with DotNodeAttributes {

  implicit object SystemDiagramToDot extends ToDotGraph[Diagram] {
    override def toDotGraph(t: Diagram): graph = graph(id = ID(t.name), kind = graph_kind.digraph,
      stmt_list = List(
        List(attr_stmt(attr_stmt_kind.graph, List(nodeAttributeToAttr(NodeAttribute.rankdir(RankDir.LR))))),
        t.nodes.map{ n =>
          node_stmt(node_id(ID(n.id), port = None),
            (n match {
              case NamedContactNode(_, name) => List(NodeAttribute.shape(ShapeKind.ellipse), NodeAttribute.label(name))
              case AnonymousContactNode(_) => List(NodeAttribute.shape(ShapeKind.point))
              case ComponentNode(_, name) => List(NodeAttribute.shape(ShapeKind.component), NodeAttribute.label(name))
            }).
              map(nodeAttributeToAttr))
        },
        t.links.map{ link =>
          edge_stmt(
            node_id(ID(link.linkId.id1.id), port = Some(port(None, compass_pt.e))),
            List(node_id(ID(link.linkId.id2.id), port = Some(port(None, compass_pt.w)))),
            link.name.toList.map(name => a_list_item(ID("label"), ID(name)))
          )
        }
      ).flatten
    )
  }
}
