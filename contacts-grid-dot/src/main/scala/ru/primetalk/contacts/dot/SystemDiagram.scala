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

  sealed trait Identity {
    val id: String
  }
  sealed trait Contact extends Identity
  case class NamedContact(id: String, name: String) extends Contact
  case class AnonymousContact(id: String) extends Contact

  /** Identifier of a link between two nodes. */
  case class LinkId(id1: Identity, id2: Identity)
  case class LinkInfo(linkId: LinkId, name: Option[String] = None)

  // , inputs: List[Contact], outputs: List[Contact]
  case class ComponentNode(id: String, name: String) extends Identity

  case class Diagram(name: String, nodes: List[Identity], links: List[LinkInfo])
}

trait SystemDiagramToDotGraph extends SystemDiagram with DotAST with DotNodeAttributes {

  implicit object SystemDiagramToDot extends ToDotGraph[Diagram] {
    override def toDotGraph(t: Diagram): graph = graph(id = ID(t.name), kind = graph_kind.digraph,
      stmt_list = List(
        t.nodes.map{ n =>
          node_stmt(node_id(ID(n.id), port = None),
            (n match {
              case NamedContact(_, name) => List(NodeAttribute.shape(ShapeKind.ellipse), NodeAttribute.label(name))
              case AnonymousContact(_) => List(NodeAttribute.shape(ShapeKind.point))
              case ComponentNode(_, name) => List(NodeAttribute.shape(ShapeKind.component), NodeAttribute.label(name))
            }).
              map(nodeAttributeToAttr))
        },
        t.links.map{ link =>
          edge_stmt(
            node_id(ID(link.linkId.id1.id), port = Some(port(None, compass_pt.e))),
            List(node_id(ID(link.linkId.id2.id), port = Some(port(None, compass_pt.w))))
          )
        }
      ).flatten
    )
  }
}
