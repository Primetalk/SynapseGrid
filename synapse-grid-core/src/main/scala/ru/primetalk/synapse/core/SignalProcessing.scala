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
 * Created: 18.03.2013
 */
package ru.primetalk.synapse.core

import scala.language.existentials
import scala.Predef._
import scala.collection.mutable

/** This contact is used to enable special simultaneous processing of signals. */
object TrellisContact extends Contact[List[Signal[_]]]

/** An incapsulation of signal for a subsystem. */
case class SubsystemDirectSignal(subsystemName:String, signal:Signal[_])
/** This contact is used to process signals of internal system.
  *
  * In asynchronous execution the resulting signal should come
  * at the same level of "call stack". However as far as we usually get the signal asynchronously
  * it is processed at top level. So in order to run it in inside the subsystem,
  * we package asynchronous result into
  * Signal(SubsystemSpecialContact, SubsystemDirectSignal( name, actual resulting signal))
  */
object SubsystemSpecialContact extends Contact[SubsystemDirectSignal]

/** A runtime system is a representation of the system that is
  * organized by Contacts and is ready for direct processing of TrellisElement.*/
case class RuntimeSystem(name:String,
                         signalProcessors: ContactToSubscribersMap,
                         stopContacts: Set[Contact[_]]
//                         ,
//                         stateHandles:List[Contact[_]]
                          )

/** A component that does single step along the trellis.*/
case class TrellisProducerSpeedy(runtimeSystem:RuntimeSystem)
  extends TrellisProducer {
  import runtimeSystem._
  def apply(t: TrellisElement): TrellisElement = {
    val signals = t._2
    //			val (outputs, inners) = t._2.partition(s ⇒ system.isOutputContact(s._1))
    val toProcess = new Signal(TrellisContact, signals) :: signals //inners

    var newState = t._1
    val newSignals = mutable.ListBuffer[Signal[_]]() // : Signals
    for {
      signal ← toProcess
      c = signal.contact
    } if (stopContacts.contains(c))
      newSignals += signal
    else
      for (proc ← signalProcessors(c)) {
        try {

          proc match {
            case RuntimeComponentHeavy(_, f) =>
              val (ctx, signals) = f(newState, signal)
              newState = ctx
              newSignals ++= signals //reverse_::: newSignals
            case RuntimeComponentLightweight(_, f) =>
              val signals = f(signal)
//              newState = newState ++ upd
              newSignals ++= signals //reverse_::: newSignals
            case RuntimeComponentStateful(_, sh, f) =>
              val s = newState(sh)
              val (ns, signals) = f(s, signal)
              newState = newState.updated(sh, ns)
              newSignals ++= signals //reverse_::: newSignals

          }
        } catch {
          case e: Exception => throw new RuntimeException(
            s"Exception ${e.getClass.getSimpleName} in handler during processing '$signal' in system '$name'.\n" +
              s"Context value before processing:\n"+newState.mkString("\n"),e)
        }
      }

    (newState, newSignals.toList)
  }
}
/** Generates trellis until there are some data on nonStop contacts.
  * Can also process signals from child subsystems (not constrained only to input contacts).*/
case class TrellisProducerLoopy(trellisProducer: TrellisProducer,
                                stopContacts: Set[Contact[_]]) extends ((Context, Signal[_])=>TrellisElement) {
	private def from(t0: TrellisElement): Stream[TrellisElement] =
		t0 #:: from(trellisProducer(t0))

  def apply(context: Context, signal: Signal[_]): TrellisElement =
    try {
      from((context, List(signal))).
        find {t =>
          val signals = t._2
          signals.forall(signal => stopContacts.contains(signal.contact))
      }.get
    } catch {
      case e: Exception =>
        throw new RuntimeException(
          s"Exception ${e.getClass.getSimpleName} during trellis rendering starting with '$signal'. " +
            s"Context value before processing:\n" + context.mkString("\n"), e)
    }
}

///**
// * Processes signals for the given system.
// * @author А.Жижелев
// *
// */
//class SignalProcessorOld(system: StaticSystem,
//                      inContacts: Set[Contact[_]], stopContacts: Set[Contact[_]])
//	extends RuntimeComponent {
////	val mapContactsToProcessors =
////    SystemConverting.systemToSignalProcessors(List(),system,
////      SystemConverting.componentToSignalProcessor)
//	val step = TrellisProducerSpeedy(SystemConverting.toRuntimeSystem(system,  //.name, mapContactsToProcessors,
//	   stopContacts)): TrellisProducer
//	val processInnerSignals = TrellisProducerLoopy(step, stopContacts)
//
//	def apply(context: Map[Contact[_], _], signal: Signal[_]): TrellisElement = {
//		if (!inContacts.contains(signal.contact))
//			throw new IllegalArgumentException(
//				s"The system ${system.name} does not have appropriate input contacts for signal: $signal.")
//
//		processInnerSignals(context, signal)
//	}
//}
///**
// * Processes signals for the given system.
// * @author А.Жижелев
// */
//class SignalProcessor(runtimeSystem:RuntimeSystem, //mapContactsToProcessors: ContactToSubscribersMap,
////											name:String,
//                      inContacts: Set[Contact[_]]
////                      stopContacts: Set[Contact[_]]
//                       )
//	extends RuntimeComponent {
//
//	val step = TrellisProducerSpeedy(runtimeSystem): TrellisProducer
//
//	val processInnerSignals = TrellisProducerLoopy(step, runtimeSystem.stopContacts)
//
//  def assertIsInInputs(signal:Signal[_]){
//    if (!inContacts.contains(signal.contact))
//      throw new IllegalArgumentException(
//        s"The system ${runtimeSystem.name} does not have appropriate input contacts for signal: $signal.")
//  }
//	def apply(context: Map[Contact[_], _], signal: Signal[_]): TrellisElement = {
//		processInnerSignals(context, signal)
//	}
//
//}
//


//    type TrellisElement = (Map[Contact[_], _], List[Signal[_]])
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
//    /** This version of step has slightly better performance than the previous one. It decreases the number of intermediate objects created. */
//    private def stepSpeedy(t: TrellisElement): TrellisElement = {
//      val signals = t._2
//      //			val (outputs, inners) = t._2.partition(s ⇒ system.isOutputContact(s._1))
//      val toProcess = new Signal(TrellisContact, signals) :: signals //inners
//
//      var newState = t._1
//      var newSignals = List[Signal[_]]() // : Signals
//      for {
//        signal ← toProcess
//        c = signal.contact
//      } if (stopContacts.contains(c))
//        newSignals = signal :: newSignals
//      else
//        for (proc ← mapContactsToProcessors(c)) {
//          try {
//            val (ctx, signals) = proc.apply(newState, signal)
//            newState = ctx
//            newSignals = signals reverse_::: newSignals
//          } catch {
//            case e: Exception => throw new RuntimeException(s"Exception ${e.getClass.getSimpleName} in handler during processing $signal.")
//          }
//        }
//
//      (newState, newSignals.reverse)
//    }

//    private def from(t0: TrellisElement): Stream[TrellisElement] =
//      t0 #:: from(step(t0))
//    /** Can process signals from child subsystems. */
//    def processInnerSignals(context: Map[Contact[_], _], signal: Signal[_]): (Map[Contact[_], _], List[Signal[_]]) = {
//      //			from((context, List(signal))).filter(t ⇒ (t._2.map(_._1).toSet.intersect(processedContacts)).isEmpty).head
//      from((context, List(signal))).filter(t ⇒ (t._2.map(_._1).toSet -- stopContacts).isEmpty).head
//    }
