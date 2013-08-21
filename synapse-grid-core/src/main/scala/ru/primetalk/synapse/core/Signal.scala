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
 * Created: 21.08.13, zhizhelev
 */
package ru.primetalk.synapse.core

/**
 * Signal is a pair of contact and data on it.
 */
case class Signal[T](contact: Contact[T], data: T) {
  val _1 = contact
  val _2 = data
}
