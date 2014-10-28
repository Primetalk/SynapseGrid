package ru.primetalk.typed.expressions

/**
 * Parsers that can match a substring of a stream of words.
 *
 * @author zhizhelev, 21.10.14.
 */
trait Parsers extends Numerals4 {

  sealed trait ParseResult[+T] {
    def isSuccess:Boolean
    def isFailure = !isSuccess
    def orElse[U >: T](other: => ParseResult[U]): ParseResult[U]
    def value:T
    def flatMap[T2](f: (T) => ParseResult[T2]): ParseResult[T2]
    def map[T2](f:T => T2): ParseResult[T2]
    def tail:LemmaStream
    def next[E](parser:Parser[E])(implicit ev:T<:<List[E]):ParseResult[List[E]]
  }

  case class Success[+T](value: T, tail:LemmaStream) extends ParseResult[T] {
    def isSuccess = true
    def orElse[U >: T](other: => ParseResult[U]): ParseResult[U] = this
    def reason:String = throw new IllegalStateException("Success have no reason to fail")
    def flatMap[T2](f: (T) => ParseResult[T2]): ParseResult[T2] = f(value)
    def map[T2](f:T => T2): ParseResult[T2] = Success(f(value), tail)
    def next[E](parser:Parser[E])(implicit ev:T<:<List[E]):ParseResult[List[E]] =
      parser(tail).map(r => r :: value)
  }

  case class Failure[+T]() extends ParseResult[T] {
    def isSuccess = false
    def orElse[U >: T](other: => ParseResult[U]): ParseResult[U] = other
    def value:T = throw new IllegalStateException("No value")
    def tail:LemmaStream = throw new IllegalStateException("No tail")
    def flatMap[T2](f: (T) => ParseResult[T2]): ParseResult[T2] = Failure()
    def map[T2](f:T => T2): ParseResult[T2] = Failure()
    def next[E](parser:Parser[E])(implicit ev:T<:<List[E]):ParseResult[List[E]] = Failure()
  }

  object ParseResult {
    def success[T](value: T, tail:LemmaStream):ParseResult[T] = Success(value, tail)
    def fail[T]:ParseResult[T] = Failure()
  }
  /** A parser takes an expression and a stream of lemmas. Returns the
    * result of matching the expression over the beginning of the input stream.
    * tparam U - result type
    */
  type Parser[U] = LemmaStream => ParseResult[U]
  type ParserFactory[U] = Expression[LemmaStream, U] => Parser[U]
  type SimpleParser[T] = String => T

  /** checks that the stream starts with the prefix and return the
    * tail of the stream. */
  def startsWithAndTail(prefix: LemmaStream, it: LemmaStream): ParseResult[Boolean] =
    if (prefix.isEmpty) Success(true, it)
    else if (it.isEmpty || prefix.head != it.head) Failure()
    else
      startsWithAndTail(prefix.tail, it.tail)

  def startsWith[T](prefix: Iterable[T], it: Iterable[T]): Boolean =
    prefix.isEmpty || (
      it.nonEmpty &&
        prefix.head == it.head &&
        startsWith(prefix.tail, it.tail)
      )


  // combines the given arguments according to sequencer
  type SequencerHandler[U] = (Any) => (U, U) => U

  def defaultSequencerHandler(sequencer: Any)(l: Long, r: Long) = sequencer match {
    case SumSequencer => l + r
    case MulSequencer => l * r
  }

  def backTrackingParser[U](sequencerHandler: SequencerHandler[U] = defaultSequencerHandler _)(e: Expression[LemmaStream, U]): Parser[U] = e match {
    case Epsilon(u) => s => Success(u, s)
    case ConstExpr(l, u) =>
      (s: LemmaStream) => startsWithAndTail(l, s).map(t=>u)
    case SelectorMap(lst) =>
      val parsers =
        lst.map(backTrackingParser(sequencerHandler)).toStream
      (s: LemmaStream) =>
        parsers.
          map(parser => parser(s)).
          dropWhile(_.isFailure).
          headOption.getOrElse(Failure())
    case s@Selector2(_, e1, e2) =>
      val lst = s.exprs.asInstanceOf[Iterable[Expression[LemmaStream, U]]]
      val parsers =
        lst.map(backTrackingParser(sequencerHandler)).toStream
      (s: LemmaStream) =>
        parsers.
          map(parser => parser(s)).
          dropWhile(_.isFailure).
          headOption.getOrElse(Failure())
    case Pair(sequencer, e1: Expression[LemmaStream, U], e2: Expression[LemmaStream, U]) =>
      val parsers: List[Parser[U]] = backTrackingParser(sequencerHandler)(e1) :: backTrackingParser(sequencerHandler)(e2) :: Nil
      (s: LemmaStream) =>
        val res = parsers.foldLeft(Success[List[U]](Nil, s):ParseResult[List[U]])(_.next(_))
        res.map{lst => val list = lst.reverse; sequencerHandler(sequencer)(list.head, list.tail.head)}
    case Pair2(e1: Expression[_, _], e2: Expression[_, _]) =>
      val parsers: List[Parser[U]] =
        backTrackingParser(sequencerHandler)(e1.asInstanceOf[Expression[LemmaStream, U]]) ::
          backTrackingParser(sequencerHandler)(e2.asInstanceOf[Expression[LemmaStream, U]]) :: Nil
      (s: LemmaStream) =>
        val res = parsers.foldLeft(Success[List[U]](Nil, s): ParseResult[List[U]])(_.next(_))
        res.map { lst => val list = lst.reverse; (list.head, list.tail.head).asInstanceOf[U]}
    case Labelled(_, expr) =>
      backTrackingParser(sequencerHandler)(expr)
    case Transformed(innerExpression: Expression[_, _], t) =>
      val innerParser = backTrackingParser(sequencerHandler.asInstanceOf[SequencerHandler[Any]])(innerExpression)
      val converter: Any => U = t.asInstanceOf[Any] match {
        case ModSplit(_) => {
          case (l: Long, r: Long) => (l + r).asInstanceOf[U]
        }: (Any => U)
        case OrderSplit(order) => {
          case ((l: Long, o: Long), r: Long) if o == order => (l * o + r).asInstanceOf[U]
        }: (Any => U)
      }
      (s: LemmaStream) =>
        innerParser(s).map(converter)
    case _ => throw new IllegalArgumentException(s"backTrackingParser is not implemented for expression $e")
  }

  def wordToLemma(word:String):Lemma
  implicit def toSimpleParser[T](p:Parser[T]):SimpleParser[T] = (text:String) => {
    val res = p(text.split(" ").map(wordToLemma))
    if(res.tail.nonEmpty)
      throw new IllegalArgumentException(s"Cannot parse '$text'")
    res.value
  }

}
