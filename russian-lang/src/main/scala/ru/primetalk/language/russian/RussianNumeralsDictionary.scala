//////////////////////////////////////////////////////////////
// Речевой портал                                           //
// © ООО «Праймтолк», 2011-2013                             //
// Авторы: Жижелев А.А., Нехаев А.Р.                        // 
// Все права принадлежат компании ООО «Праймтолк».          //
//////////////////////////////////////////////////////////////
/**
 * Speech portal
 * © Primetalk Ltd., 2011-2013.
 * Authors: Zhizhelev A., Nehaev A.
 * All rights reserved.
 * Created: 29.01.2013
 */

package ru.primetalk.language.russian

import ru.primetalk.language.russian.morphology._
import scala.Array.canBuildFrom
import ru.primetalk.language.russian.morphology.Wordform
import ru.primetalk.language.russian.morphology.WordFormDescription
import ru.primetalk.language.russian.morphology.GrammarCategoryValue
import LemmaGrammarCategory._

import scala.language.implicitConversions
/**
 * @author А.Жижелев
 *
 */
trait RussianNumeralsDictionaryT {

  import NumberLemmaClassification._
  import Gender._
  import GrammaticNumber._
  import GrammaticCase._
  import ConcordanceGroup._
  import NumeralType._
  import ExactNumeralValue._

  private val spaceR = " ".r

  implicit def split(spaceSeparatedList: String): Array[String] = {
    spaceR.split(spaceSeparatedList)
  }

  def arithmeticProgression(start: Long, stop: Long, step: Long = 1) = (start to stop by step).toList

  def geometricProgression(start: Long, step: Long, count: Int) = (for (i ← 0 until count) yield start * math.pow(step, i).toLong).toList

  def associate(words: Array[String], numbers: Iterable[Long], wordForm: WordFormDescription): List[Wordform] = {
    words.zip(numbers).map(p => WordFormAssociation(p._1,
      wordForm.overrideBy(List(NumericalValue(p._2), NumericalLemma(p._2))))).toList
  }

  val orderWordForms = List(
    simpleNumericalWordForm(ThousandOrders, Singular, Nominative),
    simpleNumericalWordForm(ThousandOrders, Singular, Genetive),
    simpleNumericalWordForm(ThousandOrders, Plural, Genetive))


  def associateOrder(words: Array[String], number: Long, gender: GrammarCategoryValue = Masculine): List[Wordform] =
    words.toList.zip(orderWordForms).
      map(p => WordFormAssociation(p._1, p._2.overrideBy(new WordFormDescription(gender, number)))).toList

  lazy val allWordForms: List[Wordform] = {
    associate("три четыре пять шесть семь восемь девять",
      3L to 9, new WordFormDescription(Masculine, Cardinal, Nominative, Ones)) :::
      associate("ноль",
        List(0L), new WordFormDescription(Cardinal, Nominative, Ones)) :::
      associate("одна две",
        List(1, 2), new WordFormDescription(Femini, Cardinal, Nominative, Ones)) :::
      associate("один два",
        List(1, 2), new WordFormDescription(Masculine, Cardinal, Nominative, Ones)) :::
      associate("одно",
        List(1), simpleNumericalWordForm(Ones, Neuter)) :::
      associate("десять одиннадцать двенадцать тринадцать четырнадцать " +
        "пятнадцать шестнадцать семнадцать восемнадцать девятнадцать",
        10L to 19, simpleNumericalWordForm(Teens)) :::
      associate("двадцать тридцать сорок пятьдесят шестьдесят семьдесят восемьдесят девяносто",
        20L to 90 by 10, simpleNumericalWordForm(Tens)) :::
      associate("сто двести триста четыреста пятьсот шестьсот семьсот восемьсот девятьсот",
        100L to 900 by 100, simpleNumericalWordForm(Hundreds)) :::
      associate("нулевой первый второй третий четвёртый пятый шестой седьмой восьмой девятый",
        0L to 9, new WordFormDescription(Masculine, Ordinal, Nominative, Ones)) :::
      associate("десятый одиннадцатый двенадцатый тринадцатый четырнадцатый пятнадцатый шестнадцатый семнадцатый восемнадцатый девятнадцатый",
        10L to 19, new WordFormDescription(Masculine, Ordinal, Nominative, Teens)) :::
      associate("нулевого первого второго третьего четвёртого пятого шестого седьмого восьмого девятого",
        0L to 9, new WordFormDescription(Masculine, Ordinal, Genetive, Ones)) :::
      associate("десятого одиннадцатого двенадцатого тринадцатого четырнадцатого пятнадцатого шестнадцатого семнадцатого восемнадцатого девятнадцатого",
        10L to 19, new WordFormDescription(Masculine, Ordinal, Genetive, Teens)) :::
      associateOrder("тысяча тысячи тысяч", 1000L, Femini) :::
      associateOrder("миллион миллиона миллионов", 1000000L) :::
      associateOrder("миллиард миллиарда миллиардов", 1000000000L) :::
      associateOrder("триллион триллиона триллионов", 1000000000000L) :::
      associate("минус", List(-1L), WordFormDescription.empty)
  }
  //	lazy val numberLemmas = {
  //		LemmaInfo(NumericalLemma(1L), Ones)
  //	}
  lazy val allNumberForms = (allWordForms map {
    case WordFormAssociation(text, WordFormDescription.Numerical(v) )  ⇒ (text, v)
  }).toMap

  lazy val textToWordSemantics = WordFormDictionaries.textToWordSemantics(allWordForms)
  lazy val wordSemanticsToText = WordFormDictionaries.wordSemanticsToText(allWordForms)

  def getConcordanceWordForm(lemma: Long): WordFormDescription = lemma match {
    case 1L ⇒ ConcordanceGroup1.wordForm
    case n: Long if n >= 2L && n <= 4L ⇒ ConcordanceGroup2_4.wordForm
    case _ ⇒ ConcordanceGroup5_more.wordForm
  }

  lazy val allNumberLemmas = (allWordForms map {
    case WordFormAssociation(text, WordFormDescription.Numerical(v)) ⇒ (text, WordFormDescription.Numerical(v))
  }).toMap
  lazy val wordFormsMap = allNumberForms.toList.groupBy(_._2).map(p ⇒ (p._1, p._2.map(_._1))).toMap.withDefault(i ⇒ List())

  def getWordForms(i: Long): List[String] =
    wordFormsMap(i)

  def getSomeWordForms(wfd: WordFormDescription) = {
    val appropriateWordForms =
      allWordForms.filter(wf ⇒ wf.wordform.isMoreSpecific(wfd) && wf.lemma.isInstanceOf[NumericalLemma])
    (i: Long) ⇒
      appropriateWordForms.withFilter(wf ⇒ wf.lemma.asInstanceOf[NumericalLemma].value == i).map(_.text)

  }

  private class Memoize[X, Y](f: X => Y) extends (X => Y) {
    val cache = scala.collection.mutable.Map[X, Y]()

    def apply(x: X) = cache.getOrElseUpdate(x, f(x))
  }

  def memoize[X, Y](f: X => Y): X => Y = new Memoize(f)

  val getExactWordForm = memoize((wfd: WordFormDescription) ⇒ {
    val appropriateWordForms = allWordForms.collect {
      case wf@WordFormAssociation(text, form) if form.matches(wfd) && form.lemma.isInstanceOf[NumericalLemma] ⇒
        val i = form.lemma.asInstanceOf[NumericalLemma].value
        (i, text)
    }
    val exacts = appropriateWordForms.groupBy(_._1).map(p ⇒ (p._1, p._2.head._2)).toMap
    (i: Long) ⇒ exacts(i)
  }
  )
  lazy val cardinalNominatives = getExactWordForm(cardinalNominativeMas)
  lazy val wordToLemma =
    allWordForms.collect {
      case wf@WordFormAssociation(text, wd) ⇒
        (text, wd.lemma)
    }.toMap

  lazy val lemmaToWords =
    allWordForms.collect {
      case wf@WordFormAssociation(text, wd) ⇒
        (wd.lemma, wf)
    }.groupBy(_._1).map(p => (p._1, p._2.map(_._2))).toMap


  implicit def toLemma(stm:String):Lemma =
    wordToLemma(stm)
}

trait RussianNumeralsT extends RussianNumeralsDictionaryT {

}

object RussianNumerals extends RussianNumeralsT