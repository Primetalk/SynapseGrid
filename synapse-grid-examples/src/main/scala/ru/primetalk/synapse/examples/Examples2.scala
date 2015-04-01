///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 *
 * Created: 17.07.13, zhizhelev
 */
package ru.primetalk.synapse.examples

import ru.primetalk.synapse.core._

object Examples2 {
  object StringSplitterBuilder extends SystemBuilder {
    implicit val sb1 = this
    val a = contact[String]("a")
    val b = contact[String]("b")
    val c = contact[Char]("c")

    inputs(a)
    outputs(b,c)
    a -> b flatMap (_.split("\\s+"))
    a -> c flatMap (_.toCharArray.toSeq)
  }

}
