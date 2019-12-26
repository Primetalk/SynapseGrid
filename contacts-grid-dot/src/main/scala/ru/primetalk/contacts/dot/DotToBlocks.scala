package ru.primetalk.contacts.dot

trait DotToBlocks extends DotAST {
  trait Show[T] {
    def show(t: T): String
  }
  implicit class ShowOps[T: Show](t: T) {
    def show: String = implicitly[Show[T]].show(t)
  }
  implicit class BooleanOps(b: Boolean) {
    def toOption: Option[Unit] = if(b) Some(()) else None
    def toList: List[Unit] = if(b) List(()) else Nil
  }
  implicit object ShowGraphKind extends Show[graph_kind] {
    override def show(t: graph_kind): String = t match {
      case graph_kind.digraph => "digraph"
      case graph_kind.graph => "graph"
    }
  }
  def wrapID(id: ID): String = {
    "\"" + id.replaceAll("\"", "\\\"") + "\""
  }
  implicit object ShowGraph extends Show[graph] {
    override def show(t: graph): String = t match {
      case graph(strict, kind, id, stmt_list) =>
        (strict.toList.map(_ => "strict") :+ kind.show :+ wrapID(id)).mkString(" ")
    }
  }
}
