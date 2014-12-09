///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011                                   //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2014.
 * All rights reserved.
 *
 * Created: 26.11.14, zhizhelev
 */
package ru.primetalk.synapse.examples

import ru.primetalk.synapse.core.SystemBuilder

import scala.language.implicitConversions

object Examples4 {
  object SentenceSplitterBuilder extends SystemBuilder  {
    val sentence = input[String]("sentence")
    val words = input[String]("words")
    val wordsInner = auxContact[String]

    (sentence -> wordsInner).flatMap (_.split("\\s+"), "split")
    words >> wordsInner

    val allCaps = output[String]("allCaps")
    val allLower = output[String]("allLower")

    (wordsInner -> allCaps).filter(_.matches("[A-Z]+"), "[A-Z]+")
    (wordsInner -> allLower).filter(_.matches("[a-z]+"), "[a-z]+")
  }

}
