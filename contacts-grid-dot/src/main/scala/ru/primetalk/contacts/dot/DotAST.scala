package ru.primetalk.contacts.dot

/** This trait contains DotAST definitions.
  *
  * The grammar is obtained from:
  * https://www.graphviz.org/doc/info/lang.html
  *
  */
trait DotAST {

  //graph 	: 	[ strict ] (graph | digraph) [ ID ] '{' stmt_list '}'
  sealed trait graph_kind
  object graph_kind {
    case object graph extends graph_kind
    case object digraph extends graph_kind
  }
  sealed trait IDTag
  // An ID is one of the following:
  // - Any string of alphabetic ([a-zA-Z\200-\377]) characters, underscores ('_') or digits ([0-9]), not beginning with a digit;
  // - a numeral [-]?(.[0-9]+ | [0-9]+(.[0-9]*)? );
  // - any double-quoted string ("...") possibly containing escaped quotes (\")1;
  // - an HTML string (<...>).
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
  case class edge_stmt(lhs: edgeLHS, edgeRHS: edgeRHS, attr_list: attr_list = Nil) extends stmt
  sealed trait edgeLHS

  //edgeRHS 	: 	edgeop (node_id | subgraph) [ edgeRHS ]
  type edgeRHS = List[edgeLHS]
  //node_stmt 	: 	node_id [ attr_list ]
  // attr_list might be empty
  case class node_stmt(node_id: node_id, attr_list: attr_list) extends stmt
  //node_id 	: 	ID [ port ]
  case class node_id(id: ID, port: Option[port]) extends edgeLHS

  //port 	: 	':' ID [ ':' compass_pt ]
  case class port(id: Option[ID], compass_pt: compass_pt)
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
    val names: Seq[String] = Seq("n", "ne", "e", "se", "s", "sw", "w", "nw", "c", "_")
    val valueToName: Map[compass_pt, String] = values.zip(names).toMap

  }
  // typeclass that defines conversion of a type to dot graph
  trait ToDotGraph[T] {
    def toDotGraph(t: T): graph
  }
  implicit class ToDotGraphOps[T: ToDotGraph](t: T){
    def toDotGraph: graph = implicitly[ToDotGraph[T]].toDotGraph(t)
  }
}

trait DotNodeAttributes extends DotAST {
  sealed trait ShapeKind
  object ShapeKind {
    case object point extends ShapeKind
    case object rectangle extends ShapeKind
    case object ellipse extends ShapeKind
    case object component extends ShapeKind
    case object tab extends ShapeKind
    case object octagon extends ShapeKind
    case object invhouse extends ShapeKind
    val values: Seq[ShapeKind] = Seq(point, rectangle, ellipse, component, tab, octagon, invhouse)
    val names: Seq[String] = Seq("point", "rectangle", "ellipse", "component", "tab", "octagon", "invhouse")
    val valueToName: Map[ShapeKind, String] = values.zip(names).toMap
  }

  sealed trait NodeAttribute
  object NodeAttribute {
    case class label(label: String) extends NodeAttribute
    case class shape(shapeKind: ShapeKind) extends NodeAttribute
    case class color(c: Color) extends NodeAttribute
    case class fontcolor(c: Color) extends NodeAttribute
    case class fillcolor(c: Color) extends NodeAttribute
    case class style(styles: List[NodeStyle]) extends NodeAttribute
    case class rankdir(rankdir: RankDir) extends NodeAttribute
  }
  sealed trait Color
  case class NamedColor(colorName: String) extends Color
  val red = NamedColor("red")
  val aquamarine = NamedColor("aquamarine")
  val cyan = NamedColor("cyan")
  val mistyrose = NamedColor("mistyrose")
  val violetred = NamedColor("violetred")
  val azure = NamedColor("azure")
  val lightpink = NamedColor("lightpink")

  def colorToID(color: Color): ID = ID(color match {
    case NamedColor(colorName) => colorName
  })
  sealed trait NodeStyle
  object NodeStyle {
    case object rounded extends NodeStyle
    case object filled extends NodeStyle
    val values: Seq[NodeStyle] = Seq(rounded, filled)
    val names: Seq[String] = Seq("rounded", "filled")
    val valueToName: Map[NodeStyle, String] = values.zip(names).toMap
  }

  sealed trait RankDir
  object RankDir {
    case object LR extends RankDir
    case object TD extends RankDir
  }
  def nodeAttributeToAttr(nodeAttribute: NodeAttribute): a_list_item = nodeAttribute match {
    case NodeAttribute.label(label) => a_list_item(ID("label"), ID(label))
    case NodeAttribute.style(s) => a_list_item(ID("style"), ID(s.map(NodeStyle.valueToName).mkString(",")))
    case NodeAttribute.shape(shapeKind) => a_list_item(ID("shape"), ID(ShapeKind.valueToName(shapeKind)))
    case NodeAttribute.color(c) => a_list_item(ID("color"), ID(c match {
      case NamedColor(colorName) => colorName
    }))
    case NodeAttribute.fillcolor(c) => a_list_item(ID("fillcolor"), colorToID(c))
    case NodeAttribute.fontcolor(c) => a_list_item(ID("fontcolor"), colorToID(c))
    case NodeAttribute.rankdir(rd) => a_list_item(ID("rankdir"), ID(rd match {
      case RankDir.LR => "LR"
      case RankDir.TD => "TD"
    }))
  }
  // rankdir = LR
  // rank=same
}
