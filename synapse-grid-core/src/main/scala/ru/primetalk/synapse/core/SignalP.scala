///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev
 *
 * Created: 04.04.14, zhizhelev
 */
package ru.primetalk.synapse.core

/**
 * Contact reference with respect to path of the system where the contact resides.
 * @param path path to a system with the contact
 * @param contact the original contact of the system
 */
case class ContactP[T](path: SystemPath, contact: Contact[T])

/**
 * Signal with path to the contact.
 * @param contact contactP
 * @param data the  value at the contact
 * @tparam T value type
 */
case class SignalP[T](contact: ContactP[T], data: T)
