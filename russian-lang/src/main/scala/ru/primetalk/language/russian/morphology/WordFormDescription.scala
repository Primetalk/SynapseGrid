package ru.primetalk.language.russian.morphology

/**
 * Морфологическая форма. Содержит ряд значений грамматических категорий,
 * однозначно адресующих конкретную словоформу для заданной леммы.
 */
case class WordFormDescription(values: List[GrammarCategoryValue]) {

  //, default:List[GrammarCategoryValue])
  def this(v: GrammarCategoryValue*) = this(v.toList)

  lazy val attributes: Map[GrammarCategory, GrammarCategoryValue] =
    values.map(v => (v.category, v)).toMap
  lazy val valuesSet = values.toSet
  lazy val categories = attributes.keySet

  def isSupersetOf(other: WordFormDescription) = {
    values.forall(value => other.valuesSet.contains(value))
  }

  def isMoreSpecific(other: WordFormDescription) =
    other.isSupersetOf(this)

  def overrideBy(other: WordFormDescription) = {
    val keptKeys = categories -- other.categories
    WordFormDescription((values.filter(v => keptKeys.contains(v.category)) ++ other.values).distinct)
  }

  def overrideBy(other: List[GrammarCategoryValue]) = {
    val keptKeys = categories -- other.map(_.category)
    WordFormDescription((values.filter(v => keptKeys.contains(v.category)) ++ other).distinct)
  }

  def subtract(other: WordFormDescription) =
    WordFormDescription(values.filterNot(other.valuesSet.contains))

  /** Either this form  contains the same value or doesn't contain the given category at all. */
  def matches(other: WordFormDescription) = {
    other.values.forall(v => valuesSet.contains(v) || !categories.contains(v.category))
  }

  lazy val lemma = LemmaGrammarCategory.getValue(this).asInstanceOf[LemmaGrammarCategory.Lemma]
}

/**
 * Словоформа — конкретное выражение лексемы в определённой форме.
 * text — lowercased
 */
case class WordFormAssociation(text: String, wordform: WordFormDescription) {
  require(text == text.toLowerCase, "text should be lower cased (" + text + ")")

  def lemma = wordform.lemma

  lazy val form = wordform.subtract(new WordFormDescription(lemma))
}


/**
 * @author zhizhelev, 22.10.14.
 */
object WordFormDescription {
  val empty = WordFormDescription(Nil)

  implicit def valuesToWordFormDescription(iterable: Iterable[GrammarCategoryValue]) = WordFormDescription(iterable.toList)

  object Numerical {
    def apply(i: Long) = new WordFormDescription(LemmaGrammarCategory.NumericalLemma(i))

    def unapply(wd: WordFormDescription): Option[Long] =
      wd.lemma match {
        case LemmaGrammarCategory.NumericalLemma(v) => Some(v)
        case _ => None
      }
  }

}
