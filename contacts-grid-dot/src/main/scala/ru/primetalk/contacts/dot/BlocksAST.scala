package ru.primetalk.contacts.dot

import ToIndentedLines._
/**
  * It's a simple low-level representation of dot language as words, blocks and lines.
  */
trait BlocksAST {
  type Word = String
  type Words = List[String]
  sealed trait BlockElement
  case class Line(line: String) extends BlockElement
  type BlockElements = List[BlockElement]
  sealed trait BraceKind
  case object CurlyBraces extends BraceKind
  case object SquareBraces extends BraceKind
  case object AngleBraces extends BraceKind
  case class TitledBlock(title: Words, braceKind: BraceKind = CurlyBraces, elements: BlockElements) extends BlockElement
}
trait BlocksShow extends BlocksAST {
  def braces(braceKind: BraceKind): (String, String) = braceKind match {
    case CurlyBraces => ("{", "}")
    case SquareBraces => ("[", "]")
    case AngleBraces => ("<", ">")
  }
  implicit object ToIndentedLinesTitledBlock extends ToIndentedLines[TitledBlock] {
    override def toIndentedLines(t: TitledBlock): List[IndentedLine] =
      IndentedLine(t.title.mkString(" ") + " " + braces(t.braceKind)._1 + "\n") :: (
        t.elements.flatMap {
          case Line(line) => List(IndentedLine(line, 1))
          case t2: TitledBlock => ToIndentedLinesTitledBlock.toIndentedLines(t2).map(_.indent)
        } :+
          IndentedLine(braces(t.braceKind)._2)
        )
  }
  implicit object ToIndentedLinesBlockElement extends ToIndentedLines[BlockElement] {
    override def toIndentedLines(t: BlockElement): List[IndentedLine] = t match {
      case Line(line) => List(IndentedLine(line))
      case tb@TitledBlock(_,_,_) => tb.toIndentedLines
    }
  }
}