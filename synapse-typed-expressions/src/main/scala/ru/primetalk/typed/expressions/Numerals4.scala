package ru.primetalk.typed.expressions

import scala.math.ScalaNumber

/**
 * Defines russian numerals construction rules.
 * @author zhizhelev, 16.10.14.
 */
trait Numerals4 {

  ///////////////////////////
  // Deferred declarations
  ///////////////////////////

  type Lemma // the type of word identifier. The lowest level
  /** The sequence of lemmas that we parse or synthesize. */
  type LemmaStream = Iterable[Lemma]
  type Num <: ScalaNumber
  /** Numeral expression corresponds to either a part of a LemmaStream or to some Long number.*/
  type NE = Expression[LemmaStream, Long]

  def lemmasForNumber(n:Long):LemmaStream


  type SemanticTag = Any


  /** A transformer that converts lower level to upper level and vice versa. */
  sealed trait Transformer[L, U]

  sealed trait Expression[L,U]
  /** Directly maps a single lower element to a single upper element.
    * @param lower the concrete value
    * @param upper the abstract value
    */
  case class ConstExpression[L, U](lower: L, upper: U) extends Expression[L, U]

  sealed trait SemanticSelector[U]

  case class EqualsSelector[U](value: U) extends SemanticSelector[U]

  case class LessThanSelector[U](lt: U) extends SemanticSelector[U]

  /** Selects either e1 or e2 depending on the semantics.
    * When parsing it is considered as e1 | e2.
    * */
  case class BooleanAlternative[L, U](selector: SemanticSelector[U], e1: Expression[L, U], e2: Expression[L, U]) extends Expression[L, U]


  /** Selects one of the given expressions.
    * When parsing it is considered as e(0) | e(1) | ... | e(size-1) */
  case class MapAlternative[L, U](exprs: Iterable[ConstExpression[L, U]]) extends Expression[L, U]

  case class Pair[L, U1, U2](e1: Expression[L, U1], e2: Expression[L, U2]) extends Expression[L, (U1, U2)]

  /** Matches empty substream and returns default.
    * Generates nothing. */
  case class Epsilon[L, U](default: U) extends Expression[L, U]

  /** Marks an expression with the label. */
  case class Labelled[L, U](label: String, e: Expression[L, U]) extends Expression[L, U]

  /** Applies the given transformation to the expression. */
  case class Transformed[L, M, U](e: Expression[L, M], t: Transformer[M, U]) extends Expression[L, U]

  /** Creates a direct mapping between a number and a lemma stream that consists of only a single lemma.*/
  implicit def mapSingleWordNumber(numbers: Iterable[Long]): NE = MapAlternative(numbers map mapSingleNumber)

  implicit def mapSingleNumber(n: Long): ConstExpression[LemmaStream, Long] = ConstExpression(lemmasForNumber(n), n)


  /** Divide a number into quotient and residue */
  case class ModSplit(module: Long) extends Transformer[(Long, Long), Long]

  /** Divide a number by order. Put the order in the second part of the resulting pair. */
  case class OrderSplit(order: Long) extends Transformer[(Long, Long), Long]

  implicit class ExprAdv[L, M](e: Expression[L, M]) {

    def ~[M2](other: Expression[L, M2]) = Pair(e, other)

    def ^^[U](t: Transformer[M, U]) = Transformed(e, t)

    def |?(defaultValue: M) = BooleanAlternative(EqualsSelector(defaultValue), e, Epsilon[L, M](defaultValue))

    def |(other: Expression[L, M]) = new {
      def selectBy(selector: SemanticSelector[M]): Expression[L, M] =
        BooleanAlternative(selector, e, other)
    }

    def labelled(label: String): Expression[L, M] = Labelled(label, e)
  }


  val `[0]` = 0L: NE
  val `[1..9]` = 1L to 9L:NE
  val `[10..19]` = 10L to 19L:NE
  val `[1..19]` = 1L to 19L:NE
  val `[20..90/10]` = 20L to 90L by 10L:NE
  val `[20..99]` = `[20..90/10]` ~ (`[1..9]` |? 0L) ^^ ModSplit(10L)
  val `[1..99]` = `[20..99]` | `[1..19]` selectBy LessThanSelector(20L)


  val `[100..900/100]` = 100L to 900 by 100:NE

  val `[100..999]` = `[100..900/100]` ~ (`[1..99]` |? 0L) ^^ ModSplit(100L)
  val `[1..999]` = `[100..999]` | `[1..99]` selectBy LessThanSelector(100L) labelled "1..999"
  val `[0..999]` = `[1..999]` | `[0]` selectBy LessThanSelector(1L)

  /* For minutes. */
  val `[20..50/10]` = 20L to 50 by 10:NE
  val `[20..59]` = `[20..50/10]` ~ (`[1..9]` |? 0L) ^^ ModSplit(10L)
  val `[1..59]` = `[20..59]` | `[1..19]` selectBy LessThanSelector(20L) labelled "1..59"

  /* For hours.*/
  val `[1..3]` = 1L to 3:NE
  val `[20..23]` = (20L: NE) ~ (`[1..3]` |? 0L) ^^ ModSplit(10L)
  val `[1..23]` = `[20..23]` | `[1..19]` selectBy LessThanSelector(20L)
  val `[0..23]` = `[1..23]` | `[0]` selectBy LessThanSelector(1L)

  /* Thousands and higher*/
  val `[1 000]` = 1000L:NE
  val `[1 000..999 000/1000]` = `[1..999]` ~ `[1 000]` ^^ OrderSplit(1000L)
  val `[1 000..999 999]` = `[1 000..999 000/1000]` ~ (`[1..999]` |? 0L) ^^ ModSplit(1000L)
  val `[1..999 999]` = `[1 000..999 999]` | `[1..999]` selectBy LessThanSelector(1000L)

  val `[1 000 000]` = 1000000L:NE
  val `[1 000 000..999 999 999]` = (`[1..999]` ~ `[1 000 000]` ^^ OrderSplit(1000000L)) ~ (`[1..999 999]` |? 0L) ^^ ModSplit(1000000L)

  val `[1..999 999 999]` = `[1 000 000..999 999 999]` | `[1..999 999]` selectBy LessThanSelector(1000000L)


  /** Number in range 1 .. 1000*order-1.
    * The expression is created with a simple recursive pattern.
    */
  def range1To999Order(order:Long):NE = order match {
    case 1L => `[1..999]`
    case o if o >= 1000 =>
      val lower = range1To999Order(order / 1000)
      val upper = (`[1..999]` ~ order ^^ OrderSplit(order)) ~ (lower |? 0L) ^^ ModSplit(order)
      upper | lower selectBy LessThanSelector(order)
    case _ => throw new IllegalArgumentException(s"Cannot construct expression for $order. Only multiple thousand orders are supported.")
  }
}
