package ru.primetalk.typed.expressions

import org.scalatest.FunSuite
import ru.primetalk.language.russian.morphology.LemmaGrammarCategory
import ru.primetalk.language.russian.{Concordancer, RussianNumerals}

/**
 * @author zhizhelev, 24.10.14.
 */
class GeneratorTests extends FunSuite {

  trait MyGenerator extends Generators {
    override type Lemma = LemmaGrammarCategory.Lemma

    override def lemmasForNumber(n: Long): LemmaStream =
      Seq(LemmaGrammarCategory.NumericalLemma(n))

    val gen: Generator[Long]

    val concordancer = new Concordancer(RussianNumerals.allWordForms)

    def generate(l: Long): String = {
      val gen1: LemmaStream = gen(l)
      val concordanced = concordancer.concordance(gen1)
      concordanced.mkString(" ")
    }

  }

  test("single word") {
    new MyGenerator {
      override val gen: Generator[Long] = constructGenerator(numeralTransformers, numeralSelectors)(`[0]`)
      assert(generate(0L) === "ноль")
    }
  }
  test("1..99") {
    new MyGenerator {
      override val gen: Generator[Long] = constructGenerator(numeralTransformers, numeralSelectors)(`[1..99]`)
      assert(generate(81L) === "восемьдесят один")
    }
  }

  test("1..999 999") {
    new MyGenerator {
      override val gen: Generator[Long] = constructGenerator(numeralTransformers, numeralSelectors)(`[1..999 999]`)
      assert(generate(12345L) === "двенадцать тысяч триста сорок пять")
    }
  }
  test("millions") {
    new MyGenerator {
      override val gen: Generator[Long] = constructGenerator(numeralTransformers, numeralSelectors)(range1To999Order(1e6.toLong))
      assert(generate(27003245L) === "двадцать семь миллионов три тысячи двести сорок пять")
    }
  }

  val generator = new MyGenerator {
    override val gen: Generator[Long] = constructGenerator(numeralTransformers, numeralSelectors)(range1To999Order(1e6.toLong))
  }

  test("a few numbers") {
    assert(generator.generate(1) === "один")
    assert(generator.generate(1000) === "одна тысяча")
    assert(generator.generate(4000000) === "четыре миллиона")
  }

}
