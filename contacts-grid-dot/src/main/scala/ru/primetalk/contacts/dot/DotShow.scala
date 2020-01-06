package ru.primetalk.contacts.dot

import Show._

import scala.util.matching.Regex

trait DotShow extends DotAST {
  implicit object ShowGraphKind extends Show[graph_kind] {
    override def show(t: graph_kind): String = t match {
      case graph_kind.digraph => "digraph"
      case graph_kind.graph => "graph"
    }
  }
  val simpleId: Regex = "[a-zA-Z_]([a-zA-Z_0-9])*".r
  val numeralId: Regex = "[-]?(.[0-9]+ | [0-9]+(.[0-9]*)? )".r
  // An ID is one of the following:
  // - Any string of alphabetic ([a-zA-Z\200-\377]) characters, underscores ('_') or digits ([0-9]), not beginning with a digit;
  // - a numeral [-]?(.[0-9]+ | [0-9]+(.[0-9]*)? );
  // - any double-quoted string ("...") possibly containing escaped quotes (\")1;
  // - an HTML string (<...>).
  def wrapID(id: ID): String = id match {
    case `simpleId`(_) => id
    case `numeralId`(_) => id
    case _ =>
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
    override def show(t: node_id): String = wrapID(t.id) + t.port.map(_.show).getOrElse("")
  }
  implicit object ShowAttrList extends Show[attr_list] {
    override def show(t: attr_list): String = if(t.isEmpty) "" else t.map{
      case a_list_item(id1, id2) => wrapID(id1) + " = " + wrapID(id2)}.mkString("[", "; ", "]")
  }

}
