package ru.primetalk.contacts.dot

import Show._

trait DotShow extends DotAST {
  implicit object ShowGraphKind extends Show[graph_kind] {
    override def show(t: graph_kind): String = t match {
      case graph_kind.digraph => "digraph"
      case graph_kind.graph => "graph"
    }
  }
  def wrapID(id: ID): String = {
    "\"" + id.replaceAll("\"", "\\\"") + "\""
  }
//  implicit object ShowGraph extends Show[graph] {
//    override def show(t: graph): String = t match {
//      case graph(strict, kind, id, stmt_list) =>
//        (Option.when(strict)("strict").toList :+ kind.show :+ wrapID(id)).mkString(" ")
//    }
//  }
  implicit object ShowPort extends Show[port] {
    override def show(t: port): String =
      t.id.map(i => ":" + wrapID(i)).getOrElse("") + ":" + compass_pt.valueToName(t.compass_pt)
  }
  implicit object ShowNodeId extends Show[node_id] {
    override def show(t: node_id): String = wrapID(t.id) + t.port.map(":" + _.show)
  }
  implicit object ShowAttrList extends Show[attr_list] {
    override def show(t: attr_list): String = if(t.isEmpty) "" else t.map{
      case a_list_item(id1, id2) => wrapID(id1) + " = " + wrapID(id2)}.mkString("[", "; ", "]")
  }

}
