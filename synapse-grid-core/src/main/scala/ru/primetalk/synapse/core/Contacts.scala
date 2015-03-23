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

sealed trait ContactStyle

case object NormalContact extends ContactStyle

case object AuxiliaryContact extends ContactStyle

case object DevNullContact extends ContactStyle

case object StateContact extends ContactStyle

/**
 * Basis point of connection of other elements.
 * If auxiliary then it is drawn on the graph as a simple little circle
 *
 * NB: the contact is not very well serializable. After deserialization
 * we obtain a different instance of the contact. But in most cases the comparison is done by
 * referential equality (.eq) and it won't do well.
 *
 * In synapse-grid-akka there is a solution for Contact serializations.
 * @see ru.primetalk.synapse.akka.ContactSerializer.
 */
class Contact[T](name1: String = null, val contactStyle: ContactStyle = NormalContact) extends Named with Serializable {
  val name = if (name1 == null) getClass.getSimpleName.replaceAllLiterally("$", "") else name1

  override def toString = "C(" + name + ")"
}

/** Contact with runtime type information.
  * Can be used for static analysis of the system.*/
class RttiContact[T](name1: String = null, contactStyle: ContactStyle = NormalContact)(implicit val classTag:scala.reflect.ClassTag[T]) extends Contact[T](name1, contactStyle)

object Contact {
  def unapply(c: Any): Option[(String, ContactStyle)] =
    c match {
      case contact: Contact[_] => Some(contact.name, contact.contactStyle)
      case _ => None
    }
}

/**
 * Signal is a pair of contact and data on it.
 * Two methods are provided to match those of pairs - _1 and _2.
 */
case class Signal[T](contact: Contact[T], data: T) {
  def _1 = contact

  def _2 = data
}


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

/** Signal for remote transfer. The real contacts are not quite well serializable (see Contact for details).
  * Thus we use the number of the contact in system's index.
  */
case class SignalDist(contactId: Int, data: AnyRef)

/** Sometimes signals are processed as a batch of data on the same contact.
  * The Batch can be used interchangeably with List[Signal[T]] */
case class Batch[T](contact: Contact[T], data: List[T]) {
  def _1 = contact

  lazy val signals = data.map(Signal(contact, _))
}

object Batch {
  implicit def batchToSignals[T](b: Batch[T]): List[Signal[T]] = b.signals
}


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
class StateHandle[S](name: String, val s0: S) extends Contact[S](name, StateContact) with Stateful[S] {
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


/** The trace of a signal towards the original one.
  *
  * @param signalsReversed a list of signals starting from the last produced one and
  *                        collecting the signals that have lead to the production of the last signal.
  * @param processorsReversed a list of processors that have worked for the production of the current signal.
  *                   The length of the processors list is usually by one shorter than the length of the signals.
  *                   However if it is a "lost trace" (the one that didn't produce output), then the last processor
  *                   is added but the signal is not added. Thus the lengths are the same.
  */
case class Trace(signalsReversed:List[Signal[_]], processorsReversed:List[RuntimeComponent] = Nil){
  def this(signal:Signal[_]) = this(List(signal))
  def signal = signalsReversed.head
}



