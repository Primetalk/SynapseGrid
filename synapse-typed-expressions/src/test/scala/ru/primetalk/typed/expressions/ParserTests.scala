package ru.primetalk.typed.expressions

import org.scalatest.FunSuite
import ru.primetalk.language.russian.RussianNumerals
import ru.primetalk.language.russian.morphology.LemmaGrammarCategory

/**
 * @author zhizhelev, 21.10.14.
 */
class ParserTests extends FunSuite {

  trait MyParsers extends Parsers {

    override type Lemma = LemmaGrammarCategory.Lemma

    override def wordToLemma(word:String) = RussianNumerals.toLemma(word)

    override def lemmasForNumber(n: Long): LemmaStream =
      Seq(LemmaGrammarCategory.NumericalLemma(n))

    val p:Parser[Long]

    lazy val parser:SimpleParser[Long] = p

    def parse(text:String):Long = parser(text)
  }

  test("simple direct mapping") {
    new MyParsers {

      val p = backTrackingParser()(`[0]`)

      val res = p(Seq("ноль").map(wordToLemma))

      assert(res === Success(0L, Seq()))
    }
  }
  test("Small numbers") {
    new MyParsers {
      val p = backTrackingParser()(`[1..19]`)
      val res = parse("пять")
      assert(res === 5L)
    }
  }
  test("two digit numbers") {
    new MyParsers {

      val p = backTrackingParser()(`[1..99]`)

      assert(parse("двадцать семь") === 27L)
      assert(parse("тридцать") === 30L)
    }
  }
  test("thousands") {
    new MyParsers {

      val p = backTrackingParser()(`[1..999 999]`)

      assert(parse("двадцать семь тысяч двести одиннадцать") === 27211L)
      assert(parse("одна тысяча") === 1000L)
    }
  }
  test("millions") {
    new MyParsers {

      val p = backTrackingParser()(range1To999Order(1e6.toLong))

      assert(parse("двадцать семь миллионов три тысячи двести сорок пять") === 27003245L)
      assert(parse("одна тысяча") === 1000L)
    }
  }
  case class A[T](value:T)
  case class B[T](value:T)

  case class MyPair[T<:Int](a:A[T], b:B[T])

  def swap[T](p:Any): (A[Int], B[Int]) =
    p match {
      case MyPair(a,b) =>
        (A(b.value), B(a.value))
    }

  test("case "){

    val x = MyPair[Int](A(1),B(2))
    val y = swap(x)

    assert(y._1.value === 2)
  }


}
