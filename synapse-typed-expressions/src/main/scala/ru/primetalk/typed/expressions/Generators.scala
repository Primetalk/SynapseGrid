package ru.primetalk.typed.expressions

/**
 * @author zhizhelev, 24.10.14.
 */
trait Generators extends Numerals4 {

  type Generator[-T] = T => LemmaStream
  type SimpleGenerator[T] = T => String

  def applyIfNotZero(g: Generator[Long])(remainder: Long): LemmaStream =
    if (remainder == 0) Iterable[Lemma]() else g(remainder)

  type TransformerGenerator[U] = Transformer[Any, U] => U => Any

  def numeralTransformers(t: Transformer[_, Long]): Long => Any = t match {
    case ModSplit(m) =>
      (u) => (u / m * m, u % m)
    case OrderSplit(order) =>
      (u) => ((u / order, order), u % order)
    case _ => throw new IllegalArgumentException(s"Unknown transformer $t")
  }

  def numeralSelectors(s: Selector[Long]): Long => Boolean = s match {
    case EqualsSelector(defaultValue) => _ == defaultValue
    case LessThanSelector(boundary) => _ < boundary
  }

  type BooleanSelectorGenerator[U] = Selector[U] => U => Boolean

  def constructGenerator[U](tGen: TransformerGenerator[U], selGen: BooleanSelectorGenerator[U])(e: Expression[LemmaStream, U]): Generator[U] = {
    implicit def uncheckedGenerics[T[_], O](t: T[_]): T[O] = t.asInstanceOf[T[O]]
    implicit def uncheckedGenerics2[T[_, _], O, P](t: T[_, _]): T[O, P] = t.asInstanceOf[T[O, P]]
    //    implicit def unchecked[T](t:Any):T = t.asInstanceOf[T]
    def constructGenerator0(e: Expression[_, _]): Generator[Any] = e match {
      case ConstExpr(l: LemmaStream, u) => (t) => l
      case Labelled(_, e1) => constructGenerator0(e1)
      case Epsilon(_) => (t) => Iterable()
      case Pair(e1, e2) =>
        val g1 = constructGenerator0(e1)
        val g2 = constructGenerator0(e2)
        (u) =>
          val (u1, u2) = u
          g1(u1) ++ g2(u2)

      case BinarySelector(sel, e1, e2) =>
        val selector = selGen(sel).asInstanceOf[Any => Boolean]
        val g1 = constructGenerator0(e1)
        val g2 = constructGenerator0(e2)
        (u) =>
          if (selector(u))
            g2(u)
          else
            g1(u)
      case MapSelector(lst) =>
        val map = lst.map(c => (c.upper, c.lower)).toMap: Map[Any, LemmaStream]
        (u: Any) =>
          map.getOrElse(u, throw new IllegalArgumentException(s"Cannot generate text for $u by $e"))
      case Transformed(innerExpression, t) =>
        val innerGen = constructGenerator0(innerExpression)
        val transformer = tGen(t)
        (u: U) => innerGen(transformer(u))

      case _ => throw new IllegalArgumentException(s"constructGenerator is not implemented for expression $e")
    }
    constructGenerator0(e)
  }
}
