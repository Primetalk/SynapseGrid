package ru.primetalk.synapse.core.components

import ru.primetalk.synapse.core.dsl.ContactsDsl

import scala.language.implicitConversions
/**
 * @author zhizhelev, 25.03.15.
 */
trait SignalsApi extends ContactsDsl {
  /**
   * Extractor of contacts' data from result.
   */
  implicit class ContactExtractor[T](c: Contact[T]) {
    /** construct a signal on this contact. */
    def signal(d: T) = Signal(c, d)

    def createSignal(d: T) = Signal(c, d)
    /** Create signals from the given data sequence. */
    def createSignals(ds: T*): List[Signal[T]] = ds.map(Signal(c, _)).toList

    /** projection of the list of signals over the contact. Only the
      * data on the contact is retained.
      * see also #filterFunction */
    def get(signals: List[Signal[_]]): List[T] = {
      val C = c
      signals.collect {
        case Signal(C, data) => data.asInstanceOf[T]
      }
    }

    def filterFunction = (signals: SignalCollection[Signal[_]]) ⇒ signals.filter(_._1 == c).map(_.asInstanceOf[Signal[T]])

    def filterNotFunction = (signals: SignalCollection[Signal[_]]) ⇒ signals.filterNot(_._1 == c)
  }

  implicit class RichSignalList(signals: Seq[Signal[_]]) {
    /** Divides the list of signals. The first part will contain signals on the given contact.
      * the second — the rest signals. */
    def partition[T](c: Contact[T]): (Seq[Signal[T]], Seq[Signal[_]]) =
      signals.
        partition(_.contact == c).
        asInstanceOf[(Seq[Signal[T]], Seq[Signal[_]])]

    def get[T](`c`: Contact[T]): Seq[T] =
      signals.
        collect {
        case Signal(`c`, data) =>
          data.asInstanceOf[T]
      }


  }

  /** One may use notation (contact -> data) to represent a signal*/
  implicit def pairToSignal[T](p: (Contact[T], T)): Signal[T] = Signal(p._1, p._2)

  sealed trait SubsystemDirectSignal0 {
    val subsystemName: String
  }

  object SubsystemDirectSignal0 {
    def unapply(s:SubsystemDirectSignal0):Option[String] = Some(s.subsystemName)
  }
//  import scala.language.existentials
  /** An encapsulation of the signal that targets a subsystem's internal contact. */
  case class SubsystemDirectSignal[T](subsystemName: String, signal: Signal[T]) extends SubsystemDirectSignal0

  case class SubsystemDirectSignalDist(subsystemName: String, signal: SignalDist) extends SubsystemDirectSignal0

  /** This contact is used to process signals of internal system.
    *
    * In asynchronous execution the resulting signal should come
    * at the same level of "call stack". However as far as we usually get the signal asynchronously
    * it is processed at top level. So in order to run it in inside the subsystem,
    * we package asynchronous result into
    * Signal(SubsystemSpecialContact, SubsystemDirectSignal( name, actual resulting signal))
    */
  object SubsystemSpecialContact extends Contact[SubsystemDirectSignal0]

  /** This contact is used to process answers of internal system. */
  object SubsystemSpecialAnswerContact extends Contact[SubsystemDirectSignal0]


}
