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
package ru.primetalk.synapse.core.runtime

import ru.primetalk.synapse.core.{Contact, Signal, SignalDist,
ContactToSubscribersMap,
UnhandledProcessingExceptionHandler, defaultUnhandledExceptionHandler,
Context, TrellisProducer, TrellisElement, TotalTrellisProducer}

import scala.Predef._
import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.existentials

/** This contact is used to enable special simultaneous processing of signals.
  * For instance the contact can be used for debug purposes.
  * */
object TrellisContact extends Contact[List[Signal[_]]]

sealed trait SubsystemDirectSignal0 {
  val subsystemName: String
}

/** An encapsulation of the signal that targets a subsystem's internal contact. */
case class SubsystemDirectSignal(subsystemName: String, signal: Signal[_]) extends SubsystemDirectSignal0

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

/** A runtime system is a representation of the system that is
  * organized by Contacts and is ready for direct processing of TrellisElement.*/
case class RuntimeSystem(name:String,
                         signalProcessors: ContactToSubscribersMap,
                         stopContacts: Set[Contact[_]],
                         unhandledExceptionHandler:UnhandledProcessingExceptionHandler
                         = defaultUnhandledExceptionHandler
                          ){
  lazy val contacts = signalProcessors.keySet
  lazy val isTrellisContactUsed = contacts.contains(TrellisContact)
}

class RuntimeSystemForTrellisProcessing(val runtimeSystem:RuntimeSystem) {

  import runtimeSystem._

  def processSignal(signal:Signal[_], initialState:Context, newSignals : mutable.ListBuffer[Signal[_]]):Context= {
    val c = signal.contact
    var currentState = initialState
    for (proc ← signalProcessors(c)) {
      try {
        proc match {
          case r@ RuntimeComponentMultiState(_, _, f) =>
            val (ctx, signals) = f(currentState, signal)
            assert(ctx.keySet == currentState.keySet,
              s"RuntimeComponentHeavy $r has changed the set of keys.\n\tWas\n$currentState\n\tBecome\n$ctx")
            currentState = ctx
            newSignals ++= signals
          case RuntimeComponentFlatMap(_, _, _, f) =>
            val signals = f(signal)
            newSignals ++= signals
          case RuntimeComponentStateFlatMap(_, _, _, sh, f) =>
            val s = currentState.asInstanceOf[Map[Contact[Any], Any]](sh)
            val (ns, signals) = f.asInstanceOf[(Any, Signal[_]) => (Any, List[Signal[_]])](s, signal)
            currentState = currentState.asInstanceOf[Map[Contact[Any], Any]].updated(sh, ns).asInstanceOf[Context]
            newSignals ++= signals
        }
      } catch {
        case e:Throwable =>
          unhandledExceptionHandler(e, name, signal, currentState)
      }
    }
    currentState
  }

}
/** A component that does single step along the trellis.*/
case class TrellisProducerSpeedy(runtimeSystemForTrellisProcessing:RuntimeSystemForTrellisProcessing)
  extends TrellisProducer {
  import runtimeSystemForTrellisProcessing._
  import runtimeSystem._


  /**
    * In the implementation `var`s and `mutable`s are used.
    * This is done intentionally
    * to improve performance.
    */
  def apply(t: TrellisElement): TrellisElement = {
    val toProcess =
      if(isTrellisContactUsed)
        new Signal(TrellisContact, t._2) :: t._2 //inners
      else
        t._2


    @tailrec
    def processAllSignals(signalsToProcess:List[Signal[_]],
                          context:Context,
                          nextStepSignalsBuffer:mutable.ListBuffer[Signal[_]]):(Context, List[Signal[_]]) =
      signalsToProcess match {
        case Nil =>
          (context, nextStepSignalsBuffer.toList)
        case signal :: tail =>
          val newState =
            if (stopContacts.contains(signal.contact)){// signals on contacts from stop-list are not processed.
              nextStepSignalsBuffer += signal
              context // the state is not changed
            }else
              processSignal(signal, context, nextStepSignalsBuffer)
          processAllSignals(tail, newState, nextStepSignalsBuffer)
      }

    processAllSignals(toProcess, t._1,mutable.ListBuffer[Signal[_]]())
  }
}
/** Generates trellis until there are some data on nonStop contacts.
  * Can also process signals from child subsystems (not constrained only to input contacts).
  * Processes one signal at a time.
  * Has the same interface as RuntimeComponentMultiState.
  *
  * It works as follows. Constructs a lazy evaluated Stream of TrellisElement s (method `from`).
  * Then searches within the stream for a first element that contains only signals at stop contacts.
  * */
case class TrellisProducerLoopy(trellisProducer: TrellisProducer,
                                stopContacts: Set[Contact[_]]) extends TotalTrellisProducer {
	private def from(t0: TrellisElement): Stream[TrellisElement] =
		t0 #:: from(trellisProducer(t0))

  def apply(context: Context, signal: Signal[_]): TrellisElement =
    try {
      val finalTrellisElement = from((context, List(signal))).
        find {t =>
          val signals = t._2
          signals.forall(signal => stopContacts.contains(signal.contact))
      }.get
      //WONTFIX: put final trellis on TrellisContact. This is impractical in the current architecture.
      // as it is intended only for debug purposes, thus we don't implement it not to sacrifice performance.
      finalTrellisElement
    } catch {
      case e: Exception =>
        throw new RuntimeException(
          s"Exception ${e.getClass.getSimpleName} during trellis rendering starting with '$signal'. " +
            s"Context value before processing:\n" + context.mkString("\n"), e)
    }
}
