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

import scala.language.implicitConversions
/**
 * Signal is a pair of contact and data on it.
 */
case class Signal[T](contact: Contact[T], data: T) {
  def _1 = contact
  def _2 = data
}

/** Sometimes signals are processed as a batch of data on the same contact.
  * The Batch can be used interchangeably with List[Signal[T]]*/
case class Batch[T](contact: Contact[T], data: List[T]) {
  def _1 = contact
  lazy val signals = data.map(Signal(contact, _))
}

object Batch {
  implicit def batchToSignals[T](b:Batch[T]) = b.signals
}