package ru.primetalk.contacts.dot

/** This trait contains DotAST definitions.
  *
  * The grammar is obtained from:
  * https://www.graphviz.org/doc/info/lang.html
  *
  * */
trait DotAST {

  //graph 	: 	[ strict ] (graph | digraph) [ ID ] '{' stmt_list '}'
  sealed trait graph_kind
  object graph_kind {
    case object graph extends graph_kind
    case object digraph extends graph_kind
  }
  sealed trait IDTag
  type ID = String with IDTag
  def ID(id: String): ID = id.asInstanceOf[ID]
  case class graph(strict: Boolean = false, kind: graph_kind = graph_kind.graph, id: ID = ID("graph"), stmt_list: stmt_list)

  //stmt_list 	: 	[ stmt [ ';' ] stmt_list ]
  type stmt_list = List[stmt]
  //stmt 	: 	node_stmt
  //	| 	edge_stmt
  //	| 	attr_stmt
  //	| 	ID '=' ID
  //	| 	subgraph
  sealed trait stmt
  //attr_stmt 	: 	(graph | node | edge) attr_list
  //attr_list 	: 	'[' [ a_list ] ']' [ attr_list ]
  //a_list 	: 	ID '=' ID [ (';' | ',') ] [ a_list ]
  case class a_list_item(id1: ID, id2: ID)
  type a_list = List[a_list_item]
  type attr_list = a_list
  sealed trait attr_stmt_kind
  object attr_stmt_kind {
    case object graph extends attr_stmt_kind
    case object node extends attr_stmt_kind
    case object edge extends attr_stmt_kind
  }
  case class attr_stmt(kind: attr_stmt_kind, attr_list: attr_list) extends stmt
  //edge_stmt 	: 	(node_id | subgraph) edgeRHS [ attr_list ]
  case class edge_stmt(lhs: edgeLHS, edgeRHS: edgeRHS)
  sealed trait edgeLHS

  //edgeRHS 	: 	edgeop (node_id | subgraph) [ edgeRHS ]
  type edgeRHS = List[edgeLHS]
  //node_stmt 	: 	node_id [ attr_list ]
  case class node_stmt(node_id: node_id, attr_list: attr_list) extends stmt
  //node_id 	: 	ID [ port ]
  case class node_id(id: ID, port: Option[port]) extends edgeLHS with stmt

  //port 	: 	':' ID [ ':' compass_pt ]
  sealed trait port
  case class portId(id: ID, compass_pt: Option[compass_pt]) extends port
  case class portCompassPt(compass_pt: compass_pt)
  //	| 	':' compass_pt
  //subgraph 	: 	[ subgraph [ ID ] ] '{' stmt_list '}'
  case class subgraph(id: Option[ID], stmt_list: stmt_list) extends edgeLHS with stmt
  //compass_pt 	: 	(n | ne | e | se | s | sw | w | nw | c | _)
  sealed trait compass_pt
  object compass_pt {
    object n extends compass_pt
    object ne extends compass_pt
    object e extends compass_pt
    object se extends compass_pt
    object s extends compass_pt
    object sw extends compass_pt
    object w extends compass_pt
    object nw extends compass_pt
    object c extends compass_pt
    object `_` extends compass_pt
    val values: Seq[compass_pt] = Seq(n, ne, e, se, s, sw, w, nw, c, `_`)
  }
}
