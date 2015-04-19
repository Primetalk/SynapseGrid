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

//, Signal,
//ContactToSubscribersMap,
//UnhandledProcessingExceptionHandler, defaultUnhandledExceptionHandler,
//Context, TrellisProducer, TrellisElement, TotalTrellisProducer}

import scala.Predef._
import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.existentials

/** Deprecated because there is a SignalProcessingApi0
  * that supports both Signal processing and tracking signal processing. */
@deprecated("use SignalProcessingSimpleApi or SignalProcessingTrackingApi", "19.04.2015")
trait TrellisProducerImpl
  extends TrellisApi
  with RuntimeSystemApi {


  class RuntimeSystemForTrellisProcessing(val runtimeSystem: RuntimeSystem) {

    import runtimeSystem._

    def processSignal(signal: Signal[_], initialState: Context, newSignals: mutable.ListBuffer[Signal[_]]): Context = {
      val c = signal.contact
      var currentState = initialState
      for (proc ← signalProcessors(c)) {
        try {
          proc match {
            case r@RuntimeComponentMultiState(_, _, f) =>
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
          case e: Throwable =>
            unhandledExceptionHandler(e, name, signal, currentState)
        }
      }
      currentState
    }

  }

  /** A component that does single step along the trellis. */
  case class TrellisProducerSpeedy(runtimeSystemForTrellisProcessing: RuntimeSystemForTrellisProcessing)
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
        if (isTrellisContactUsed)
          SignalCollection(new Signal(TrellisContact, t._2)) ++: t._2 //inners
        else
          t._2


      @tailrec
      def processAllSignals(signalsToProcess: SignalCollection[Signal[_]],
                            context: Context,
                            nextStepSignalsBuffer: mutable.ListBuffer[Signal[_]]): (Context, SignalCollection[Signal[_]]) =
        signalsToProcess match {
          case Nil =>
            (context, nextStepSignalsBuffer.toList)
          case signal :: tail =>
            val newState =
              if (stopContacts.contains(signal.contact)) {
                // signals on contacts from stop-list are not processed.
                nextStepSignalsBuffer += signal
                context // the state is not changed
              } else
                processSignal(signal, context, nextStepSignalsBuffer)
            processAllSignals(tail, newState, nextStepSignalsBuffer)
        }

      processAllSignals(toProcess, t._1, mutable.ListBuffer[Signal[_]]())
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
          find { t =>
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
  //    /** Converts the runtime system to a RuntimeComponentHeavy that does all inner processing in a single outer step. */
  //    def toTotalTrellisProducerOld: TotalTrellisProducer = {
  //      val rsftp = new RuntimeSystemForTrellisProcessing(runtimeSystem)
  //      val step = TrellisProducerSpeedy(rsftp)
  //      val loopy = TrellisProducerLoopy(step, runtimeSystem.stopContacts)
  //      loopy
  //    }

}