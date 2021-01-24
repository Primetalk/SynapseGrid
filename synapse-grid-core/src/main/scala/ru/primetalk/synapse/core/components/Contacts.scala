/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 * (2-clause BSD license) See LICENSE
 *
 * Created: 14.03.2013
 */
package ru.primetalk.synapse.core.components

///**
// * Contact reference with respect to path of the system where the contact resides.
// * @param path path to a system with the contact
// * @param contact the original contact of the system
// */
//case class ContactP[T](path: SystemPath, contact: Contact[T])

///**
// * Signal with path to the contact.
// * @param contact contactP
// * @param data the  value at the contact
// * @tparam T value type
// */
//case class SignalP[T](contact: ContactP[T], data: T)
//
///** Sometimes signals are processed as a batch of data on the same contact.
//  * The Batch can be used interchangeably with List[Signal[T]] */
//case class Batch[T](contact: Contact[T], data: List[T]) {
//  def _1 = contact
//
//  lazy val signals = data.map(Signal(contact, _))
//}
//
//object Batch {
//  implicit def batchToSignals[T](b: Batch[T]): List[Signal[T]] = b.signals
//}











