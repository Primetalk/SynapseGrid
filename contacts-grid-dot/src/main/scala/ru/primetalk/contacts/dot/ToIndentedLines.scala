package ru.primetalk.contacts.dot

case class IndentedLine(line: String, i: Int = 0) {
  def indent: IndentedLine = copy(i = i + 1)
}

trait ToIndentedLines[T] {
  def toIndentedLines(t: T): List[IndentedLine]
}

object ToIndentedLines {
  implicit class ToIndentedLinesOps[T: ToIndentedLines](t: T) {
    def toIndentedLines: List[IndentedLine] = implicitly[ToIndentedLines[T]].toIndentedLines(t)
  }
  implicit object ShowListOfIndentedLines extends Show[List[IndentedLine]] {
    override def show(t: List[IndentedLine]): String =
      t.
        map{
          case IndentedLine(l, i) =>
            List.fill(i)("  ").mkString + l + "\n"
        }.
        mkString
  }
}

