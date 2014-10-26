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
    case DirectMapExpr(l: LemmaStream, u) =>
      (t: U) => l
    case Labelled(_, e1) =>
      constructGenerator(e1)
    case SelectorN(selector, Pair(SumSequencer, e1, eRemainder) :: _ :: Nil) =>
      val gRem = constructGenerator(eRemainder)
      selector match {
        case ModSplit(m) =>
          val g1 = constructGenerator(e1)
          (u: U) =>
            val l = u.asInstanceOf[Long]
            g1(l / m * m) ++ applyIfNotZero(gRem)(l % m)
        case OrderSplit(order) => e1 match {
          case Pair(MulSequencer, e3, e4) =>
            val g1 = constructGenerator(e3)
            val g2 = constructGenerator(e4)
            (u: U) =>
              val l = u.asInstanceOf[Long]
              g1(l / order) ++ g2(order) ++ applyIfNotZero(gRem)(l % order)
        }
      }
    case SelectorN(selector, lst) =>
      val arr = lst.toArray
      val generators = arr.map(constructGenerator)
      selector match {
        case SelectorByEqualsUpper =>
          val map = arr.zipWithIndex.map { case (DirectMapExpr(_, u), i) => (u, i)}.toMap
          (u: U) =>
            val i = map.getOrElse(u, throw new IllegalArgumentException(s"Cannot generate text for $u by $e"))
            generators(i)(u)
        case RangeSelector(r) =>
          (u: U) =>
            if (u.asInstanceOf[Long] >= r) generators.head(u)
            else generators.tail.head(u)
      }
    case _ => throw new IllegalArgumentException(s"constructGenerator is not implemented for expression $e")
  }
}
