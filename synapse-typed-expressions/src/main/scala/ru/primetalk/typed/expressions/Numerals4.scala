package ru.primetalk.typed.expressions

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
  /** Numeral expression corresponds to either a part of a LemmaStream or to some Long number.*/
  type NE = Expression[LemmaStream, Long]

  def lemmasForNumber(n:Long):LemmaStream

  type SemanticTag = Any
  sealed trait Expression[L,U]
  /** Directly maps a single lower element to a single upper element.
    * @param lower the concrete value
    * @param upper the abstract value
    */
  case class DirectMapExpr[L,U](lower:L, upper:U) extends Expression[L,U]
  /** Selects either e1 or e2 depending on the semantics.
    * When parsing it is considered as e1 | e2*/
//  case class Selector2[L,U](selector:SemanticTag, e1:Expression[L,U], e2:Expression[L,U]) extends Expression[L,U]
  /** Selects one of the given expressions.
    * When parsing it is considered as e(0) | e(1) | ... | e(size-1) */
  case class SelectorN[L,U](selector:SemanticTag, exprs:Iterable[Expression[L,U]]) extends Expression[L,U]

  case object SelectorByEqualsUpper

  /** Represents a sequential composition of expressions. 
    * @param sequencer is a semantic tag that describes the method of composition.
    */
  case class Pair[L,U1, U2, U](sequencer:SemanticTag, e1:Expression[L,U1], e2:Expression[L,U2]) extends Expression[L,U]

  case class Labelled[L,U](label:String, e:Expression[L,U]) extends Expression[L,U]
//  case class Pair[L,U1, U2, U[_,_]](e1:Expression[L,U1], e2:Expression[L,U2]) extends Expression[L,U[U1, U2]]
  /** Creates a direct mapping between a number and a lemma stream that consists of only a single lemma.*/
  implicit def mapSingleWordNumber(numbers:Iterable[Long]):NE = SelectorN(SelectorByEqualsUpper, numbers map mapSingleNumber)

  implicit def mapSingleNumber(n:Long):Expression[LemmaStream,Long]= DirectMapExpr(lemmasForNumber(n),n)//mapSingleWordNumber(Seq(number))

  case object MulSequencer
  case object SumSequencer

  implicit class NEAdv(e:NE){
    def ~*(other:NE):NE = Pair(MulSequencer, e, other)
    def ~+(other:NE):NE = Pair(SumSequencer, e, other)
    def ^|(other:NE) = new {
      def ^^(selector:Any):NE = SelectorN(selector, List(e, other))
    }
    def |(other:NE) = new {
      def selectBy(selector:Any):NE = SelectorN(selector, List(e, other))
    }
  }
  implicit class ExpressionAdv[L,U](e:Expression[L,U]){
    def labelled(label:String):Expression[L,U] = Labelled(label, e)
  }

  case class ModSplit(module:Long){
    def apply(higher:NE, smaller:NE):NE = higher ~+ smaller | higher selectBy this
  }
  case class OrderSplit(order:Long){
    def apply(higher:NE, smaller:NE):NE = (higher ~* (order:NE) ~+ smaller) | (higher ~* (order:NE)) selectBy this
  }
  case class RangeSelector(gte:Long){
    def apply(higher:NE, smaller:NE):NE = higher | smaller selectBy this
  }
  val `[0]` = 0L:NE
  val `[1..9]` = 1L to 9L:NE
  val `[10..19]` = 10L to 19L:NE
  val `[1..19]` = 1L to 19L:NE
  val `[20..90/10]` = 20L to 90L by 10L:NE
  val `[20..99]` = `[20..90/10]` ~+ `[1..9]` | `[20..90/10]` selectBy ModSplit(10L)
  val `[1..99]` = `[20..99]` | `[1..19]` selectBy RangeSelector(20L)


  val `[100..900/100]` = 100L to 900 by 100:NE

  val `[100..999]` = `[100..900/100]` ~+ `[1..99]` | `[100..900/100]` selectBy ModSplit(100L)
  val `[1..999]` = `[100..999]` | `[1..99]` selectBy RangeSelector(100L) labelled  "1..999"
  val `[0..999]` = `[1..999]` | `[0]` selectBy RangeSelector(1L)

  /* For minutes. */
  val `[20..50/10]` = 20L to 50 by 10:NE
  val `[20..59]` = `[20..50/10]` ~+ `[1..9]` | `[20..50/10]` selectBy ModSplit(10L)
  val `[1..59]` = `[20..59]`| `[1..19]` selectBy RangeSelector(20L) labelled  "1..59"

  /* For hours.*/
  val `[1..3]` = 1L to 3:NE
  //	val `[4..19]` = 4L to 19:TE[Long]
  val `[20..23]` = (20L : NE) ~+ `[1..3]` | (20L:NE) selectBy ModSplit(10L)
  val `[1..23]` = `[20..23]` | `[1..19]` selectBy RangeSelector(20L)
  val `[0..23]` = `[1..23]` | `[0]` selectBy RangeSelector(1L)

  /* Thousands and higher*/
  val `[1 000]` = 1000L:NE
  val `[1 000..999 999]` = (`[1..999]` ~* `[1 000]` ~+ `[1..999]`) | (`[1..999]` ~* `[1 000]` ) selectBy OrderSplit(1000L)
  val `[1..999 999]` = `[1 000..999 999]` | `[1..999]` selectBy RangeSelector(1000L)

  val `[1 000 000]` = 1000000L:NE
  val `[1 000 000..999 999 999]` = (`[1..999]` ~* `[1 000 000]` ~+ `[1..999 999]`) | (`[1..999]` ~* `[1 000 000]` ) selectBy OrderSplit(1000000L)
  val `[1..999 999 999]` = `[1 000 000..999 999 999]` | `[1..999 999]` selectBy RangeSelector(1000000L)


  /** Number in range 1 .. 1000*order-1.
    * The expression is created with a simple recursive pattern.
    */
  def range1To999Order(order:Long):NE = order match {
    case 1L => `[1..999]`
    case _ =>
      val lower = range1To999Order(order/1000)
      val ordNE = order:NE
      val upper = `[1..999]` ~* ordNE
      ((upper ~+ lower) | upper selectBy OrderSplit(order)) | lower selectBy RangeSelector(order)
  }
}
