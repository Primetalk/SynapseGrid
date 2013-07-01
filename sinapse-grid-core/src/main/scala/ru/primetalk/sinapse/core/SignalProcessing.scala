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
 * Created: 18.03.2013
 */
package ru.primetalk.sinapse.core

import scala.language.existentials

/** This contact is used to enable special simultaneous processing of signals. */
object TrellisContact extends Contact[List[Signal[_]]]

trait SignalProcessing {
  //	implicit class RichContext(val permanentSignals : List[Signal[_]]) {
  //		lazy val byContact = permanentSignals.groupBy(_._1).withDefault(c ⇒ List())
  //	}
  type Context = Map[Contact[_], _]

  /** The most general processing element.
    * Is very similar to
    * StateFlatMap */
  type InnerSignalProcessor = (Map[Contact[_], _], Signal[_]) => (Map[Contact[_], _], List[Signal[_]])

  /** Converts components to a function that will do the work when the data appears on one of the contacts. */
  def componentToInnerSignalProcessor(system: StaticSystem, c: OuterSystem): InnerSignalProcessor = c match {
    case Link(from, to, MapLink(f, _)) ⇒
      (context, signal) ⇒
        (context, List(new Signal(to, f.asInstanceOf[Any ⇒ Any](signal.data))))
    case Link(from, to, FlatMapLink(f, _)) ⇒ {
      (context: Context, signal) ⇒
        val fun = f.asInstanceOf[Any ⇒ TraversableOnce[Any]]
        val res = fun(signal.data)
        (context, res.map(new Signal(to, _)).toList)
    }
    case Link(from, to, StateZipLink(pe: Contact[_], _)) ⇒ {
      (context: Context, signal) ⇒
        val stateHandle = pe: Contact[_]
        (context, List(new Signal(to, (context(stateHandle), signal.data))))
    }
    case Link(from, to, NopLink(_)) ⇒
      (context: Context, signal) ⇒
        (context, List(new Signal(to, signal.data)))
    case Link(from, to, RedMapLink(stopContacts, _)) ⇒
      val proc = new SignalProcessor(system, Set(to), stopContacts)
      (context: Context, signal) ⇒ proc(context, Signal(to, signal.data))

    case InnerSystem(subsystem, subsystemStateHandle, sharedStateHandles) ⇒
      val proc = new SignalProcessor(subsystem, subsystem.inputContacts, subsystem.outputContacts)
      val sharedStateHandlersSet = sharedStateHandles.toSet[Contact[_]]
      val subsystemStateHandle1 = subsystemStateHandle: Contact[_]
      if (sharedStateHandlersSet.isEmpty)
        (context: Context, signal) ⇒ {
          val oldState = context(subsystemStateHandle1).asInstanceOf[Map[Contact[_], _]]
          val oldStateWithShared = oldState
          val (newState, signals) = proc(oldStateWithShared, signal)
          val newStateWithoutShared = newState
          (context updated(subsystemStateHandle1, newStateWithoutShared), signals)
        }
      else
        (context, signal) ⇒ {
          val oldState = context(subsystemStateHandle1).asInstanceOf[Map[Contact[_], _]]
          val sharedStates = sharedStateHandles.map(ssh ⇒ (ssh, context(ssh)))
          val oldStateWithShared = oldState ++ sharedStates
          val (newState, signals) = proc(oldStateWithShared, signal)
          val newStateWithoutShared = newState.filterKeys(ssh ⇒ !sharedStateHandlersSet.contains(ssh))
          val sharedStateValues = newState.filterKeys(ssh ⇒ sharedStateHandlersSet.contains(ssh))
          ((context updated(subsystemStateHandle1, newStateWithoutShared)) ++ sharedStateValues, signals)
        }
    case StateUpdate(from, pe, _, f) ⇒ {
      (context, signal) ⇒
        val stateHandle = pe: Contact[_]
        (context updated(stateHandle, f.asInstanceOf[(Any, Any) => Any](context(stateHandle), signal.data)), List())
    }
    // Deprecated. Use StateZipLink
    case Link(from, to, StatefulMapLink(f, pe, _)) ⇒
      (context, signal) ⇒ {
        val stateHandle = pe: Contact[_]
        val oldState = context(stateHandle)
        val (nState, nData) = f.asInstanceOf[(Any, Any) ⇒ (Any, Any)](oldState, signal.data)
        (context updated(stateHandle, nState), List(new Signal(to, nData)))
      }
    // Deprecated. Use StateZipLink
    case Link(from, to, StatefulFlatMapLink(f, pe, _)) ⇒
      (context, signal) ⇒ {
        val stateHandle = pe: Contact[_]
        val oldState = context(stateHandle)
        val (nState, nDatas) = f.asInstanceOf[(Any, Any) ⇒ (Any, Seq[Any])](oldState, signal.data)
        (context updated(stateHandle, nState), nDatas.toList.map(new Signal(to, _)))
      }

  }

  /**
   * Processes signals for the given system.
   * @author А.Жижелев
   *
   */
  class SignalProcessor(system: StaticSystem, inContacts: Set[Contact[_]], stopContacts: Set[Contact[_]])
    extends InnerSignalProcessor {
    lazy val mapContactsToProcessors = {
      val contactsProcessors = (
        for {
          component ← system.components
          proc = componentToInnerSignalProcessor(system, component)
          i ← component.inputContacts
        } yield (i, proc): (Contact[_], InnerSignalProcessor) // unify existential types within pair.
        ).toList

      val lst = contactsProcessors.groupBy(_._1).map(p ⇒ (p._1, p._2.map(_._2)))
      lst.toMap[Contact[_], List[InnerSignalProcessor]].withDefault(c ⇒ List())
    }

    //		private def stepLegacy(t : (Map[Contact[_], _], List[Signal[_]])) : (Map[Contact[_], _], List[Signal[_]]) = {
    //			def step0(contextAndResSignals : (Map[Contact[_], _], List[Signal[_]]), task : (InnerSignalProcessor, Signal[_])) : (Map[Contact[_], _], List[Signal[_]]) = {
    //				val (proc, signal) = task
    //				val (context, resSignals) = contextAndResSignals
    //				val result = proc(context, signal)
    //				(result._1, result._2 reverse_::: resSignals)
    //			}
    //			val (context, signals) = t
    //			val (outputs, inners) = signals.partition(s ⇒ system.isOutputContact(s._1))
    //			val toProcess = new Signal(TrellisContact, signals) :: inners
    //			/** Формирует "задания на вычисления" — совокупность компонента и данных. */
    //			val processingTasks = for {
    //				signal ← toProcess
    //				c = signal.contact
    //				proc <- mapContactsToProcessors(c)
    //			} yield (proc, signal : Signal[_])
    //
    //			val newSignalsAccumulator = List[Signal[_]]() // : Signals
    //			val (newState, newSignals) = ((context, newSignalsAccumulator) /: processingTasks) (step0)    // == processingTasks foldLeft (context, newSignalsAccumulator)
    //			(newState, newSignals reverse_::: outputs)
    //		}
    /** This version of step has slightly better performance than the previous one. It decreases the number of intermediate objects created. */
    private def stepSpeedy(t: (Map[Contact[_], _], List[Signal[_]])): (Map[Contact[_], _], List[Signal[_]]) = {
      val signals = t._2
      //			val (outputs, inners) = t._2.partition(s ⇒ system.isOutputContact(s._1))
      val toProcess = new Signal(TrellisContact, signals) :: signals //inners

      var newState = t._1
      var newSignals = List[Signal[_]]() // : Signals
      for {
        signal ← toProcess
        c = signal.contact
      } if (stopContacts.contains(c))
        newSignals = signal :: newSignals
      else
        for (proc ← mapContactsToProcessors(c)) {
          val (ctx, signals) = proc.apply(newState, signal)
          newState = ctx
          newSignals = signals reverse_::: newSignals
        }

      (newState, newSignals.reverse)
    }

    private def from(t0: (Map[Contact[_], _], List[Signal[_]])): Stream[(Map[Contact[_], _], List[Signal[_]])] =
      t0 #:: from(stepSpeedy(t0))

    def apply(context: Map[Contact[_], _], signal: Signal[_]): (Map[Contact[_], _], List[Signal[_]]) = {
      if (!inContacts.contains(signal.contact))
        throw new IllegalArgumentException(s"The system ${system.name} does not have appropriate input contacts for signal: $signal.")

      //			from((context, List(signal))).filter(t ⇒ (t._2.map(_._1).toSet.intersect(processedContacts)).isEmpty).head
      from((context, List(signal))).filter(t ⇒ (t._2.map(_._1).toSet -- stopContacts).isEmpty).head
    }
  }

}

object SignalProcessing extends SignalProcessing {
  def toDynamicSystem(s: StaticSystem) = {
    val proc = new SignalProcessor(s, s.inputContacts, s.outputContacts)
    var state = s.s0
    def receive(signal: Signal[_]): List[Signal[_]] = {
      def receive0(st: s.StateType, resSignals: List[Signal[_]], signals: List[Signal[_]]): (s.StateType, List[Signal[_]]) = signals match {
        case Nil ⇒ (st, resSignals)
        case head :: tail ⇒
          val (newState, newSignals) = proc(st, head)
          receive0(newState, newSignals reverse_::: resSignals, tail)
      }
      val result = receive0(state, Nil, signal :: Nil)
      state = result._1
      result._2.reverse
    }
    new DynamicSystem(s.inputContacts, s.outputContacts, s.name, receive)
  }
}