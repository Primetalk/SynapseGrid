package ru.primetalk.contacts.dot

import Show._

trait DotToBlocks extends DotAST with BlocksAST with DotShow {
  trait ToBlocks[T] {
    def toBlocks(t: T): BlockElement
  }
  implicit class ToBlocksOps[T: ToBlocks](t: T) {
    def toBlocks: BlockElement = implicitly[ToBlocks[T]].toBlocks(t)
  }

  def arrowForGraphKind(graphKind: graph_kind): String = graphKind match {
    case graph_kind.graph => " -- "
    case graph_kind.digraph => " -> "
  }

  def edgeLHSToBlockElements(arrow: String)(edgeLHS: edgeLHS): BlockElements = edgeLHS match {
    case n: node_id => List(Line(n.show))
    case s: subgraph => stmtToBlockElements(arrow)(s)
  }
  def stmtToBlockElements(arrow: String)(stmt: stmt): BlockElements = stmt match {
    case attr_stmt(_, attr_list) => List(Line(attr_list.show))
    case edge_stmt(n: node_id, List(n2: node_id), attrs) => List(Line(n.show + arrow + n2.show + attrs.show))
    case edge_stmt(edgeLHS, edgeRHS, attrs) =>
      edgeRHS.foldLeft(edgeLHSToBlockElements(arrow)(edgeLHS)){
        case (blockElements, item) =>
          blockElements ::: Line(arrow) :: edgeLHSToBlockElements(arrow)(item)
      } :+
        Line(attrs.show)
    case node_stmt(n: node_id, attr_list) => List(Line(n.show + attr_list.show))
    case subgraph(id, stmt_list) => List(TitledBlock(
      id.map(i => List("subgraph", wrapID(i))).getOrElse(Nil),
      braceKind = CurlyBraces,
      stmt_list.flatMap(stmtToBlockElements(arrow))
    ))
  }
  implicit object ToBlocksGraph extends ToBlocks[graph] {
    override def toBlocks(t: graph): BlockElement = t match {
      case graph(strict, kind, id, stmt_list) =>
        val arrow = arrowForGraphKind(kind)
        TitledBlock(
          (Option.when(strict)("strict").toList :+ kind.show :+ wrapID(id)),
          braceKind = CurlyBraces,
          stmt_list.flatMap(stmtToBlockElements(arrow))
        )

    }
  }
}
