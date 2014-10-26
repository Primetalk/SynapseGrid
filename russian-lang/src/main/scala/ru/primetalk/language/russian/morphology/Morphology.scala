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
 * Created: 05.01.2013
 */

package ru.primetalk.language.russian.morphology

import scala.collection.mutable

/**
 * Здесь объявлены грамматические классы/категории, позволяющие ортогонально классифицировать
 * слова по их категориям (фасетная классификация).
 * Каждая грамматическая категория представляет собой фасет, имеющий несколько взаимоисключающих значений.
 *
 * Для представления фасетов используется необычный способ моделирования. Как сама категория, так и
 * граммемы представлены объектами. Однако граммемы представлены объектами, унаследованными от типа
 * Категория.ThisGrammarCategoryValue, что позволяет при необходимости сузить тип принимаемых объектов.
 *
 * Представление категории объектом, а не классом, позволяет использовать саму категорию как ключ
 * в таблицах значений.
 * @author А.Жижелев
 *
 */

/**
 * Граммема - значение грамматической категрии. Например, для рода — мужской, женский, средний; для падежей — именительный, родительный ...
 * Внешний тип используется для того, чтобы было удобно создавать списки из нескольких граммем.
 * Внутри каждой грамматической категории имеется тип ThisGrammarCategoryValue, позволяющий сгруппировать все экземпляры
 * категории.
 *
 */
trait GrammarCategoryValue {
  val category : GrammarCategory
}

/**
 * Грамматическая категория. Например: род, падеж, склонение, число и т.п.
 */
sealed trait GrammarCategory {

  private
  val valuesBuffer = mutable.ListBuffer[Grammem]()

	trait Grammem extends GrammarCategoryValue {
		val category = GrammarCategory.this
    valuesBuffer += this
	}

	def tryGetValue(wordForm: WordFormDescription): Option[Grammem] =
		if (wordForm.attributes.keySet.contains(this))
			Some(wordForm.attributes(this).asInstanceOf[Grammem])
		else
			None

	def getValue(wordForm: WordFormDescription): Grammem =
		tryGetValue(wordForm).getOrElse(default)

	/** The most popular value that will probably fit if there is no more information. */
	val default: Grammem

  /** Список всех граммем грамматической категории. */
  def values = valuesBuffer.toList
  
  def byIndex(i:Int) = values(i)
}

/**
 * Грамматическая категория "Род"
 */
case object Gender extends GrammarCategory {
	val default = Masculine

	case object Masculine extends Grammem

	case object Femini extends Grammem

	case object Neuter extends Grammem

}

/**
 * Грамматическая категория "Число"
 */
case object GrammaticNumber extends GrammarCategory {
	val default = Singular

	case object Singular extends Grammem

	case object Plural extends Grammem

}

/**
 *
 */
case object GrammaticCase extends GrammarCategory {
	val default = Nominative

	/** Именительный */
	case object Nominative extends Grammem

	/** Родительный */
	case object Genetive extends Grammem

	/** Дательный */
	case object Dative extends Grammem

	/** Винительный */
	case object Accusative extends Grammem

	/** Творительный */
	case object Ablative extends Grammem

	/** Предложный */
	case object Prepositional extends Grammem

}


/**
 * See ru.vrn.vds.language.morphology.NumeralType
 */
case object NumeralType extends GrammarCategory {
	val default = Cardinal

	/** Количественные (один/одна, два/две, ...) */
	case object Cardinal extends Grammem

	/** Порядковые (первый, второй...) */
	case object Ordinal extends Grammem

	/** Собирательные (один, двое, трое...) */
	case object Collectives extends Grammem

	/** Кортежные (единица, двойка, тройка...) */
	case object NumbericalTuple extends Grammem

}

/** Численное значение, ассоциированное со словом.
  * Например, все числительные имеют численное значение, равное их математическому значению.
  * Буквы А,Б...Я также имеют численное значение, равное их номеру в алфавите.
  * дюжина — 12
  * гросс — 144
  * десяток — 10
  * пара — 2
  * трояк — 3
  * полтиник — 50
  * червонец — 10
  *
  *
  */
case object ExactNumeralValue extends GrammarCategory {
	val default = NoNumeralValue

	case object NoNumeralValue extends Grammem

	case class NumericalValue(value: Long) extends Grammem

}

case object NumberLemmaClassification extends GrammarCategory {
	val default = Ones

	case object Zero extends Grammem

	case object Ones extends Grammem

	case object Teens extends Grammem

	case object Tens extends Grammem

	case object Hundreds extends Grammem

	/** 1000, 10^6, 10^9 ... */
	case object ThousandOrders extends Grammem

}

case object PartOfSpeech extends GrammarCategory {
	val default = Noun

	case object Noun extends Grammem

	case object Verb extends Grammem

	case object Adjective extends Grammem

}

case object ConcordanceGroup extends GrammarCategory {
	val default = ConcordanceGroup5_more

	import GrammaticNumber._
	import GrammaticCase._

	protected class ConcordanceValue(val wordForm: WordFormDescription) extends Grammem

	case object ConcordanceGroup1 extends ConcordanceValue(WordFormDescription(List(Singular, Nominative)))

	case object ConcordanceGroup2_4 extends ConcordanceValue(WordFormDescription(List(Singular, Genetive)))

	case object ConcordanceGroup5_more extends ConcordanceValue(WordFormDescription(List(Plural, Genetive)))

}

/** Identifier of a lemma is the most important characteristics of a word. */
case object LemmaGrammarCategory extends GrammarCategory {
  /** As far as lemma identifier is a required property of a word.*/
	val default = NoLemma

	sealed trait Lemma extends Grammem

	/**
	 * Numerical lemma can be identified with a number that is represented by this lemma.
	 * 1 - one
	 * 1000 - thousand
	 *
	 * Exception:
	 * -1 - minus
	 */
	case class NumericalLemma(value: Long) extends Lemma

	/**
	 * Lemma can be identified by infinitive wordform
	 */
	case class InfinitiveLemma(infinitive: String) extends Lemma

	/**
	 * Lemma can be given with a stem (основа слова без окончания).
	 */
	case class StemLemma(stem: String) extends Lemma

	/** The lemma for a word that doesn't change. */
	case class UnmodifiedWordLemma(word: String) extends Lemma

	case object NoLemma extends Lemma

}

object GrammarCategories{

}
//	class GrammaticCharacteristic(attributes:Map[GrammarCategory, GrammarCategoryValue])
