package ru.primetalk.synapse.core

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Tracking processing collects lost and final traces during signal processing. (Lost traces are those that
 * haven't produced any result.)
 * Be careful! Collecting traces in a system with intensive processing can quickly lead to memory overflow.
 * This processing is intended for debug purposes.
 * @author zhizhelev, 20.11.14.
 */

class RuntimeSystemForTrellisProcessingTracking(val runtimeSystem:RuntimeSystem) {

  import runtimeSystem._

  def processSignal(trace:Trace, initialState:Context, newTraces : mutable.ListBuffer[Trace], lostTraces:mutable.ListBuffer[Trace]):Context= {
    val signal = trace.signal
    val c = signal.contact
    var currentState = initialState
    for (proc ← signalProcessors(c)) {
      val procs = proc::trace.processorsReversed
      def addTraces(signals:List[Signal[_]]) = signals match {
        case Nil => lostTraces += Trace(trace.signalsReversed, procs)
        case _ => newTraces ++= signals.map(s => Trace(s::trace.signalsReversed, procs))
      }
      try {
        proc match {
          case r@ RuntimeComponentMultiState(_, _, f) =>
            val (ctx, signals) = f(currentState, signal)
            assert(ctx.keySet == currentState.keySet,
              s"RuntimeComponentHeavy $r has changed the set of keys.\n\tWas\n$currentState\n\tBecome\n$ctx")
            currentState = ctx
            addTraces(signals)
          case RuntimeComponentFlatMap(_, _, _, f) =>
            val signals = f(signal)
            addTraces(signals)
          case RuntimeComponentStateFlatMap(_, _, _, sh, f) =>
            val s = currentState.asInstanceOf[Map[Contact[Any], Any]](sh)
            val (ns, signals) = f.asInstanceOf[Function2[Any, Signal[_], (Any, List[Signal[_]])]](s, signal)
            currentState = currentState.asInstanceOf[Map[Contact[Any], Any]].updated(sh, ns).asInstanceOf[Context]
            addTraces(signals)
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
case class TrellisProducerSpeedyTracking(runtimeSystemForTrellisProcessing:RuntimeSystemForTrellisProcessingTracking)
  extends TrellisProducerTracking {
  import runtimeSystemForTrellisProcessing._
  import runtimeSystem._


  /**
   * In the implementation `var`s and `mutable`s are used.
   * This is done intentionally
   * to improve performance.
   */
  def apply(t: TrellisElementTracking): TrellisElementTracking = {
    val toProcess =
      if(isTrellisContactUsed)
        new Trace(new Signal(TrellisContact, t._2.map(_.signal))) :: t._2 //inners
      else
        t._2


    @tailrec
    def processAllSignals(signalsToProcess:List[Trace],
                          context:Context,
                          nextStepSignalsBuffer:mutable.ListBuffer[Trace] = mutable.ListBuffer[Trace](),
                          lostTraces:mutable.ListBuffer[Trace] = mutable.ListBuffer[Trace]()):(Context, List[Trace]) =
      signalsToProcess match {
        case Nil =>
          (context, nextStepSignalsBuffer.toList)
        case trace :: tail =>
          val newState =
            if (stopContacts.contains(trace.signal.contact)){// signals on contacts from stop-list are not processed.
              nextStepSignalsBuffer += trace
              context // the state is not changed
            }else
              processSignal(trace, context, nextStepSignalsBuffer, lostTraces)
          processAllSignals(tail, newState, nextStepSignalsBuffer, lostTraces)
      }

    processAllSignals(toProcess, t._1)
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
case class TrellisProducerLoopyTracking(trellisProducer: TrellisProducerTracking,
                                stopContacts: Set[Contact[_]]) extends TotalTrellisProducerTracking {
  private def from(t0: TrellisElementTracking): Stream[TrellisElementTracking] =
    t0 #:: from(trellisProducer(t0))

  def apply(context: Context, signal: Signal[_]): TrellisElementTracking =
    try {
      val finalTrellisElement = from((context, List(new Trace(signal)))).
        find {t =>
        val traces = t._2
        traces.forall(trace => stopContacts.contains(trace.signal.contact))
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
