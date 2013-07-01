///////////////////////////////////////////////////////////////
// СинаптическаяСеть
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SinapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 * (2-clause BSD license) See LICENSE
 *
 * Created: 14.03.2013
 */
package ru.primetalk.sinapse.core

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
 * Stateful elements of the system
 */
trait Stateful[State] extends Named {
  type StateType = State
  /**
   * The initial state of the element
   */
  val s0: State
}

sealed trait ContactStyle

case object NormalContact extends ContactStyle

case object AuxiliaryContact extends ContactStyle

case object DevNullContact extends ContactStyle

case object StateContact extends ContactStyle

/**
 * Basis point of connection of other elements.
 * If auxiliary then it is drawn on the graph as a simple little circle
 */
class Contact[T](name1: String = null, val contactStyle: ContactStyle = NormalContact) extends Named {
  val name = if (name1 == null) getClass.getSimpleName.replaceAllLiterally("$", "") else name1

  override def toString = "C(" + name + ")"
}

object Contact {
  def unapply(c: Any): Option[(String, ContactStyle)] =
    c match {
      case contact: Contact[_] => Some(contact.name, contact.contactStyle)
      case _ => None
    }
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

/**
 * Signal is a pair of contact and data on it.
 */
case class Signal[T](contact: Contact[T], data: T) {
  val _1 = contact
  val _2 = data
}


object Contacts {
  implicit def pairToSignal[T](p: (Contact[T], T)) = Signal(p._1, p._2)

  /**
   * Extractor of contacts' data from result.
   */
  implicit class ContactExtractor[T](c: Contact[T]) {

    def createSignal(d: T) = Signal(c, d)

    def createSignals(ds: T*): List[Signal[T]] = ds.map(Signal(c, _)).toList

    def get(signals: List[Signal[_]]) :List[T]= {
      val C = c
      signals.collect{case Signal(C, data) => data.asInstanceOf[T]}
    }

    def filterFunction = (signals: List[Signal[_]]) ⇒ signals.filter(_._1 == c).map(_.asInstanceOf[Signal[T]])

    def filterNotFunction = (signals: List[Signal[_]]) ⇒ signals.filterNot(_._1 == c)
  }

}