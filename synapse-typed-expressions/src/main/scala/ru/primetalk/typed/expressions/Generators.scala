package ru.primetalk.typed.expressions

import scala.language.implicitConversions
/**
 * @author zhizhelev, 24.10.14.
 */
trait Generators extends Numerals4 {

  /** The result of a generator is always defined in contrast to Parser. If it fails for some input
    * it means that it is a bug */
  type Generator[-T] = T => LemmaStream
  type SimpleGenerator[T] = T => String

  def applyIfNotZero(g: Generator[Long])(remainder: Long): LemmaStream =
    if (remainder == 0) Iterable[Lemma]() else g(remainder)

  type TransformerGenerator[U] = Transformer[Any, U] => U => Any

  def numeralTransformers(t: Transformer[_, Long]): Long => Any = t match {
    case ModSplit(m) =>
      (u) => (u / m * m, u % m)
    case OrderSplit(order) =>
      (u) => (u / order, order)
    case _ => throw new IllegalArgumentException(s"Unknown transformer $t")
  }

  def numeralSelectors(s: SemanticSelector[Long]): Long => Boolean = s match {
    case EqualsSelector(defaultValue) => _ == defaultValue
    case LessThanSelector(boundary) => _ < boundary
  }

  type BooleanSelectorGenerator[U] = SemanticSelector[U] => U => Boolean

  def constructGenerator[U](tGen: TransformerGenerator[U], selGen: BooleanSelectorGenerator[U])(e: Expression[LemmaStream, U]): Generator[U] = {
    implicit def uncheckedGenerics[T[_], O](t: T[_]): T[O] = t.asInstanceOf[T[O]]
    implicit def uncheckedGenerics2[T[_, _], O, P](t: T[_, _]): T[O, P] = t.asInstanceOf[T[O, P]]
    def constructGenerator0(e: Expression[_, _]): Generator[Any] = e match {
      case ConstExpression(l: LemmaStream, u) => (t) => l
      case Labelled(_, e1) => constructGenerator0(e1)
      case Epsilon(_) => (t) => Iterable()
      case Pair(e1, e2) =>
        val g1 = constructGenerator0(e1)
        val g2 = constructGenerator0(e2)
        (u: (_, _)) => g1(u._1) ++ g2(u._2)
      case BooleanAlternative(sel: SemanticSelector[U], e1, e2) =>
        val selector = selGen(sel).asInstanceOf[Any => Boolean]
        val g1 = constructGenerator0(e1)
        val g2 = constructGenerator0(e2)
        (u) =>
          if (selector(u))
            g2(u)
          else
            g1(u)
      case MapAlternative(lst) =>
        val map = lst.map(c => (c.upper:Any, c.lower.asInstanceOf[LemmaStream])).toMap
        (u) =>
          map.getOrElse(u, throw new IllegalArgumentException(s"Cannot generate text for $u by $e"))
      case Transformed(innerExpression, t) =>
        val innerGen = constructGenerator0(innerExpression)
        val transformer = tGen(t.asInstanceOf[Transformer[Any, U]])
        (u: U) => innerGen(transformer(u))
      case _ => throw new IllegalArgumentException(s"constructGenerator is not implemented for expression $e")
    }
    constructGenerator0(e)
  }
}
