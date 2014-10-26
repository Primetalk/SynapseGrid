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
 * Created: 30.01.2013
 */

package ru.primetalk.language.russian

import ru.primetalk.language.russian.morphology._
import LemmaGrammarCategory._
/** Utilities for working with word form dictionaries*/
object WordFormDictionaries {
	type WordFormDictionary = List[WordFormAssociation]
	def textToWordSemantics(dic:WordFormDictionary) = dic.map(a ⇒ (a.text, a.wordform)).toMap
	def wordSemanticsToText(dic:WordFormDictionary) = dic.map(a ⇒ (a.wordform, a.text)).toMap
	def lemmaToForms(dic:WordFormDictionary) = dic.groupBy(_.wordform.lemma).toMap

	def createNounDictionary(infinitive:String)(gender: Gender.Grammem = Gender.default,
	                                            number: GrammaticNumber.Grammem = GrammaticNumber.default)(
		                        grammaticCases: String* // should have 6 elements
		                        ) = {
		val gc = grammaticCases.toList
		val inf = InfinitiveLemma(infinitive)

		gc.zipWithIndex.
			map {
			case (t, i) =>
				WordFormAssociation(t,
						WordFormDescription(List(inf,
							gender,
							number,
							GrammaticCase.byIndex(i))))
		}

	}

}
/**
 * @author А.Жижелев
 *
 */
trait WordFormDictionaryOld {
	protected val wordForms = new scala.collection.mutable.ListBuffer[WordFormAssociation]
	def addWordForm(wordForm:WordFormAssociation) {
		clearIndices()
		wordForms += wordForm
	}
	def addSomeWordForm(infinitive:String, someWordForm:String) {
		addWordForm(WordFormAssociation(someWordForm, new WordFormDescription(InfinitiveLemma(infinitive))))
	}
	/**
	 * NB: after reading allWordForms the next added word form will make a complete copy 
	 * of the dictionary. Thus it is recommended not to read until all
	 * wordforms have been added.
	 */
	def allWordForms = wordForms.toList
	/**
	 * Looks up the dictionary for infinitive of the given wordform.
	 * If cannot find — returns the word itself.
	 * If there are a few infinitives a warning is reported to log.
	 */
	def infinitive(wordform:String):String =
		getInfinitives(wordform).headOption.getOrElse(wordform)
	private var infinitivesVar : Map[String, Set[String]] = null
	/**
	 * return map from wordform to infinitive
	 */
	private def infinitives:Map[String, Set[String]] = {
		if(infinitivesVar == null){
			val grouped = (for{
				p <- wordFormIndex//.map(p => (p._2.values.flatMap(wf => wf.text), p._1, ))
				wfp <- p._2
				if wfp.lemma.isInstanceOf[InfinitiveLemma]
				infinitive = wfp.lemma.asInstanceOf[InfinitiveLemma]
			}yield (p._1, infinitive.infinitive)).groupBy(_._1).map(p => (p._1, p._2.values.toSet))
			infinitivesVar = grouped
		}
		infinitivesVar
	}
	private var wordFormsVar : Map[String, Map[WordFormDescription, List[WordFormAssociation]]] = null
	private def wordFormMaps:Map[String, Map[WordFormDescription, List[WordFormAssociation]]] = {
		if(wordFormsVar == null){
			val grouped = allWordForms.groupBy(_.lemma).
			filter(p => p._1.isInstanceOf[InfinitiveLemma]).
			map(p=>(p._1.asInstanceOf[InfinitiveLemma].infinitive, p._2.groupBy(_.form)))
			wordFormsVar = grouped
		}
		wordFormsVar
	}
	private var wordFormIndexVar : Map[String, List[WordFormAssociation]] = null
	def wordFormIndex:Map[String, List[WordFormAssociation]] = {
		if(wordFormIndexVar == null)
			wordFormIndexVar = allWordForms.groupBy(_.text)
		wordFormIndexVar
	}
	def getInfinitives(wordform:String):Set[String] = 
		infinitives(wordform)
	
	private def clearIndices(){
		infinitivesVar = null
		wordFormsVar = null
		wordFormIndexVar = null
	}
	def lemmatize(words:List[String]):List[String] = 
		words.map(w=>infinitives(w).headOption.getOrElse(w)) 
		

}