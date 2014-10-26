///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * ${PROJECT_NAME}
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 *
 * Created: 01.08.13, zhizhelev
 */
package ru.primetalk.language.russian

import scala.language.implicitConversions

package object morphology {

	implicit def catListToMap(values:List[GrammarCategoryValue]) :
		Map[GrammarCategory, GrammarCategoryValue] = {
		val v = values.asInstanceOf[List[GrammarCategoryValue]].map(v => (v.category, v))
		v.toMap//[GrammarCategory, GrammarCategoryValue[Any]]
	}
	type MorphologicalForm = WordFormDescription
	type Wordform = WordFormAssociation
	type MorphologicalDictionary = List[Wordform]
	import NumeralType._
	import GrammaticCase._
	import Gender._
	import LemmaGrammarCategory._
	val cardinalNominativeMas = new WordFormDescription(Cardinal, Nominative, Masculine)
	val cardinalNominativeFem = new WordFormDescription(Cardinal, Nominative, Femini)
	val cardinalNomNumberWordForm = new WordFormDescription(Cardinal, Nominative)
	val ordinalNomNumberWordForm = new WordFormDescription(Ordinal, Nominative)
	val ordinalGenNumberWordForm = new WordFormDescription(Ordinal, Genetive)
	private val defaultNumerical = new WordFormDescription(Cardinal, Nominative)
	def simpleNumericalWordForm(additionalAttributes:GrammarCategoryValue *) =
		defaultNumerical.overrideBy(WordFormDescription(additionalAttributes.toList))
//	private def categoriesToMap(values:GrammarCategoryValue *)  = {//: Map[GrammarCategory, GrammarCategoryValue[_]]
//		val t = List(values: _ *).map(v => (v.category, v))
//		t.toMap
//	}
	def categories(values:List[GrammarCategoryValue]) =
		values.map(_.category).toSet
	implicit def longToLemma(number:Long) = NumericalLemma(number)


	def concordance(lemmas:Iterable[Lemma])(implicit wordFormDictionary:List[WordFormAssociation]):Iterable[String] = {
		lazy val forms = WordFormDictionaries.lemmaToForms(wordFormDictionary)
		val lattice = lemmas.map(l=>forms(l))
		lattice.map(f => f.headOption.map(_.text).getOrElse("UNK"))
	}

}
