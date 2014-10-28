///////////////////////////////////////////////////////////////
// Речевой портал                                            //
// © ООО «Праймтолк», 2011-2013                              //
// Авторы: Жижелев А.А., Нехаев А.Р., Попов П.А.             // 
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * Speech portal
 * © Primetalk Ltd., 2011-2013.
 * Authors: Zhizhelev A., Nehaev A., Popov P.
 * All rights reserved.
 * Created: 14.04.2013
 */
package ru.primetalk.language.russian

import ru.primetalk.language.russian.morphology.LemmaGrammarCategory._
import ru.primetalk.language.russian.morphology._

/**
 * @author А.Жижелев
 *
 */
class Concordancer(wordFormDictionary: List[WordFormAssociation]) {

  import ru.primetalk.language.russian.morphology.Gender._
  import ru.primetalk.language.russian.morphology.GrammaticCase._
  import ru.primetalk.language.russian.morphology.GrammaticNumber._

  val masculine = new WordFormDescription(Masculine)
  val femini = new WordFormDescription(Femini)
  val singularNom = new WordFormDescription(Singular, Nominative)
  val singularGen = new WordFormDescription(Singular, Genetive)
  val pluralGen = new WordFormDescription(Plural, Genetive)

  def selectBetter(wordForms: List[WordFormAssociation], req: WordFormDescription, lemma: Lemma): WordFormAssociation = {
    if (wordForms.isEmpty)
      lemma match {
        case InfinitiveLemma(word) =>
          WordFormAssociation(word, new WordFormDescription(lemma))
        case UnmodifiedWordLemma(word) =>
          WordFormAssociation(word, new WordFormDescription(lemma))
        case _ =>
          throw new IllegalArgumentException(s"No wordforms for $req for lemma $lemma")
      }
    else
      wordForms.find(_.form.matches(req)).getOrElse(wordForms.head)

  }

  type LemmaInfo = (Lemma, List[WordFormAssociation])

  /** Selects the best form for lemma given the left and right contexts. */
  def betterForm(lemma: LemmaInfo, left: Option[LemmaInfo], right: Option[LemmaInfo]): WordFormAssociation = (left, lemma, right) match {
    // the word after numerical should have matching grammar case and number
    case (Some((NumericalLemma(n), _)), (i@InfinitiveLemma(inf), lst), _) if n % 10 == 1 && n % 100 / 10 != 1 =>
      selectBetter(lst, singularNom, i)
    case (Some((NumericalLemma(n), _)), (i@InfinitiveLemma(inf), lst), _) if n % 10 > 1 && n % 10 < 5 && n % 100 / 10 != 1 =>
      selectBetter(lst, singularGen, i)
    case (Some((NumericalLemma(n), _)), (i@InfinitiveLemma(inf), lst), _) =>
      selectBetter(lst, pluralGen, i)
    case (Some((NumericalLemma(n), _)), (l@NumericalLemma(m), lst), _) if m >= 1000 && n % 10 == 1 && n % 100 / 10 != 1 =>
      selectBetter(lst, singularNom, l)
    case (Some((NumericalLemma(n), _)), (l@NumericalLemma(m), lst), _) if m >= 1000 && n % 10 > 1 && n % 10 < 5 && n % 100 / 10 != 1 =>
      selectBetter(lst, singularGen, l)
    case (Some((NumericalLemma(n), _)), (l@NumericalLemma(m), lst), _) if m >= 1000 =>
      selectBetter(lst, pluralGen, l)
    case (_, (n: NumericalLemma, lst), None) => // the last numerical should be masculine
      selectBetter(lst, masculine, n)
    case (_, (n: NumericalLemma, lst), Some((NumericalLemma(m), _))) if m == 1000 => // the numerical before order should have matching gender
      selectBetter(lst, femini, n)
    case (_, (n: NumericalLemma, lst), Some((NumericalLemma(m), _))) if m >= 1000000 => // the numerical before order should have matching gender
      selectBetter(lst, masculine, n)
    case (_, (u@UnmodifiedWordLemma(word), _), _) =>
      selectBetter(List(), singularNom, u)
    //			WordFormAssociation(word, WordFormSemantics(u, new WordFormDescription()))
    case (_, (u@InfinitiveLemma(word), _), _) =>
      selectBetter(List(), singularNom, u)
    case (_, (n: NumericalLemma, lst), _) => // default numerical gender is masculine
      selectBetter(lst, masculine, n)
    case _ =>
      lemma._2.headOption.getOrElse(WordFormAssociation("unk", new WordFormDescription(lemma._1)))
  }

  lazy val forms = WordFormDictionaries.lemmaToForms(wordFormDictionary)

  private def arrayGetOption[T](arr: Array[T], i: Int): Option[T] = {
    if (i < 0 || i >= arr.length) None
    else Some(arr(i))
  }

  def concordance(lemmas: Iterable[Lemma]): Iterable[String] = {
    val lemmaList = lemmas.toList
    try {
      val lattice = lemmaList.map(l => (l, forms.getOrElse(l,
        //throw new IllegalArgumentException("No word forms for lemma "+l)) ))//
        List[WordFormAssociation]())))
      val arr = lattice.toArray
      val better =
        for (i <- 0 until arr.size)
        yield betterForm(arr(i), arrayGetOption(arr, i - 1), arrayGetOption(arr, i + 1))
      better.map(_.text)
    } catch {
      case e: Exception =>
        throw new RuntimeException("Couldn't find concordance for " + lemmaList, e)
    }
    //		lattice.map(f => f._2.headOption.map(_.text).getOrElse("UNK"))
  }

}