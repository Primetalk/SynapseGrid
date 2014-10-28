package ru.primetalk.typed.expressions

/**
 * @author zhizhelev, 24.10.14.
 */
trait Generators extends Numerals4 {

  type Generator[T] = T => LemmaStream
  type SimpleGenerator[T] = T => String

  def applyIfNotZero(g:Generator[Long])(remainder:Long):LemmaStream =
    if(remainder == 0) Iterable[Lemma]() else g(remainder)

  def constructGenerator[U](e: Expression[LemmaStream, U]): Generator[U] = e match {
    case ConstExpr(l: LemmaStream, u) =>
      (t: U) => l
    case Labelled(_, e1) =>
      constructGenerator(e1)

    case Pair2(e1, e2) =>
      val g1 = constructGenerator[Any](e1.asInstanceOf[Expression[LemmaStream, Any]])
      val g2 = constructGenerator[Any](e2.asInstanceOf[Expression[LemmaStream, Any]])
      (u: U) =>
        val (u1, u2) = u
        g1(u1) ++ g2(u2)
    case Selector2(OrDefault, e1, Epsilon(default)) =>
      val g1 = constructGenerator(e1)
      (u: U) => if (u == default)
        Iterable()
      else
        g1(u)
    case Selector2(RangeSelector(r), e1, e2) =>
      val generators = Array(e1, e2).map(constructGenerator)
      (u: U) =>
        if (u.asInstanceOf[Long] >= r) generators.head(u)
        else generators.tail.head(u)
    case SelectorMap(lst) =>
      val arr = lst.toArray
      val generators = arr.map(constructGenerator)
      val map = arr.zipWithIndex.map { case (ConstExpr(_, u), i) => (u, i)}.toMap
          (u: U) =>
            val i = map.getOrElse(u, throw new IllegalArgumentException(s"Cannot generate text for $u by $e"))
            generators(i)(u)
    case Transformed(innerExpression, t) =>
      val innerGen = constructGenerator(innerExpression)
      val transformer: U => Any = t.asInstanceOf[Any] match {
        case ModSplit(m) =>
          (u: U) => {
            val l = u.asInstanceOf[Long]
            (l / m * m, l % m)
          }
        case OrderSplit(order) =>
          (u: U) => {
            val l = u.asInstanceOf[Long]
            ((l / order, order), l % order)
          }
      }
      (u: U) =>
        val middle = transformer(u)
        innerGen(middle)

    case _ => throw new IllegalArgumentException(s"constructGenerator is not implemented for expression $e")
  }
}
