///////////////////////////////////////////////////////////////
// СинаптическаяСеть
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 * (2-clause BSD license) See LICENSE
 *
 * Created: 14.03.2013
 */
package ru.primetalk.synapse.core

import scala.language.implicitConversions

/**
 * Named is used to store graph specific information - label or name.
 */
trait Named {
  def name: String

  override def toString =
    getClass.getSimpleName + "(\"" + name + "\")"
}

/**
 * Basis point of connection of other elements.
 * If auxiliary then it is drawn on the graph as a simple little circle.
 *
 * It order to improve performance the contact is compared by referential equality (#eq()).
 * That's why it is not a case class. However, this creates some inconvenience in serialization.
 *
 * NB: the contact is not very well serializable. After deserialization
 * we obtain a different instance of the contact. But in most cases the comparison is done by
 * referential equality (.eq) and thus it won't do well.
 *
 * In synapse-grid-akka there is a solution for Contact serializations.
 *
 * @see ru.primetalk.synapse.akka.ContactSerializer.
 *
 */
class Contact[T](name1: String = null) extends Named with Serializable {
  val name = if (name1 == null) getClass.getSimpleName.replaceAllLiterally("$", "") else name1

  override def toString = "C(" + name + ")"
}


object Contact {
  def unapply(contact: Contact[_]): Option[String] = Some(contact.name)
}


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


/**
 * Stateful elements of the system.
 */
trait Stateful[State] extends Named {
  type StateType = State
  /**
   * The initial state of the element.
   */
  val s0: State
}

/**
 * Permanent contacts store shared state that can be updated with stateful
 * links.
 */
class StateHandle[S](name: String, val s0: S) extends Contact[S](name) with Stateful[S] {
  override def toString = "S(" + name + ")"
}

object StateHandle {
  def apply[S](name: String, s0: S) = new StateHandle(name, s0)

  def unapply(s: Any): Option[(String, _)] =
    s match {
      case stateHandle: StateHandle[_] => Some(stateHandle.name, stateHandle.s0)
      case _ => None
    }
}






