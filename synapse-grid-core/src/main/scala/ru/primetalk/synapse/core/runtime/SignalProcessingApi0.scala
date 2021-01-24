package ru.primetalk.synapse.core.runtime

import ru.primetalk.synapse.core.components.{Contact0, Signal0}
import ru.primetalk.synapse.core.dsl.SignalsApi

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.implicitConversions

trait SignalProcessingApi0 extends SignalsApi with TrellisApi with RuntimeComponentApi with RuntimeSystemApi {

  /** A generic trait for different signal processing methods.
    * There are two descendants - a tracking one and a simple one.
    * The tracking signal processor keeps track of what signals were produced during processing.
    * The simple one do not save traces. So it requires less memory for processing.
    *
    * Keeping traces can greatly help with debugging. One may see which signals and processors have lead to
    * the result. It may be worth to save trellis to dot file.
    *
    * Having a common ancestor for both types of processing is advantageous because there will
    * be no code duplication among processors.
    */
  trait SignalProcessing0 {
    /** The type that is used to represent a single value related to signal */
    type TSignal
    type TSignals = List[TSignal]
    type TrellisElement = (Context, TSignals)
    type TrellisElementTracking = (Context, TSignals)
    type TrellisProducerTracking = TotalTrellisBuilder => TSignals => TSignals
    type TotalTrellisProducerTracking = (Context, Signal0) => TrellisElementTracking

    implicit def tSignalToSignal(s: TSignal): Signal0

    def signalToTSignal(s: Signal0): TSignal

    def newTotalTrellisBuilder(runtimeSystem: RuntimeSystem, context: Context): TotalTrellisBuilder

    /** TrellisBuilder is a way for a final user to watch the process of trellis building.
      * One may override #newTotalTrellisBuilder and put own implementation of TotalTrellisBuilder.*/
    trait TotalTrellisBuilder {
      def currentState: Context

      def currentState_=(ctx: Context): Unit

      /** Saves the signal that is the last one in a trace. It didn't produce output. */
      def saveTerminatedSignal(signal: TSignal): Unit

      /** Save the signal that appeared on the stop-contact. */
      def saveStopSignal(signal: TSignal): Unit

      /** Completes the previous step and starts a new time moment.
        *
        * @return builder for the next part of the trellis.
        */
      def newSingleStepBuilder: TrellisBuilder

      /** Adds an exception to the trellis.
        * Usually calls runtimeSystem.unhandledExceptionHandler.
        * @param trace original signal that has lead to the exception
        * @param proc  signal processor
        */
      def addException(trace: TSignal, proc: RuntimeComponent, exception: Throwable): Unit

      /** The list of signals on stop contacts. */
      def stopSignals: TSignals
    }

    /** Constructs trellis.
      * After running a proc over the trace signal, we
      * construct the next step of the trellis.
      * An instance of this class is constructed anew for every trellis step.
      *
      * */
    trait TrellisBuilder {
      def currentState: Context

      def currentState_=(ctx: Context): Unit

      def addSignals(trace: TSignal, proc: RuntimeComponent, signals: SignalCollection[Signal0]): Unit

      /** Adds an exception to the trellis.
        * Usually calls runtimeSystem.unhandledExceptionHandler.
        * @param trace original signal that has lead to the exception
        * @param proc  signal processor
        */
      def addException(trace: TSignal, proc: RuntimeComponent, exception: Throwable): Unit

      def toTrellisElement: TSignals
    }

    /**
     * Tracking processing collects lost and final traces during signal processing. (Lost traces are those that
     * haven't produced any result.)
     * Be careful! Collecting traces in a system with intensive processing can quickly lead to memory overflow.
     * This processing is intended for debug purposes.
     * @author zhizhelev, 20.11.14.
     */

    class RuntimeSystemForTrellisProcessingTracking(val runtimeSystem: RuntimeSystem) {

      import runtimeSystem._

      def processSignal(trace: TSignal, trellisBuilder: TrellisBuilder): Context = {
        val signal = trace: Signal0
        val c = signal.contact
        for proc <- signalProcessors(c) do {
          try {
            proc match {
              case r@RuntimeComponentMultiState(_, _, f) =>
                val (ctx, signals) = f(trellisBuilder.currentState, signal)
                assert(ctx.keySet == trellisBuilder.currentState.keySet,
                  s"RuntimeComponentMultiState $r has changed the set of keys.\n\tWas\n${trellisBuilder.currentState}\n\tBecome\n$ctx")
                trellisBuilder.currentState = ctx
                trellisBuilder.addSignals(trace, proc, signals)
              case RuntimeComponentFlatMap(_, _, _, f) =>
                val signals = f(signal)
                trellisBuilder.addSignals(trace, proc, signals)
              case RuntimeComponentStateFlatMap(_, _, _, sh, f) =>
                val shAny = sh.asInstanceOf[Contact[Any]]
                val s = trellisBuilder.currentState.asInstanceOf[Map[Contact[Any], Any]](shAny)
                val (ns, signals) = f.asInstanceOf[(Any, Signal0) => (Any, IterableOnce[Signal0])](s, signal)
                trellisBuilder.currentState = trellisBuilder.currentState.asInstanceOf[Map[Contact[Any], Any]].updated(shAny, ns).asInstanceOf[Context]
                trellisBuilder.addSignals(trace, proc, signals)
              case _ =>
                throw new IllegalArgumentException(s"Cannot process $proc")
            }
          } catch {
            case e: Throwable =>
              trellisBuilder.currentState = unhandledExceptionHandler(e, name, signal, trellisBuilder.currentState)
          }
        }
        trellisBuilder.currentState
      }

    }

    /** A component that does single step along the trellis. */
    case class TrellisProducerSpeedyTracking(runtimeSystemForTrellisProcessing: RuntimeSystemForTrellisProcessingTracking) {

      import runtimeSystemForTrellisProcessing._
      import runtimeSystem._


      /**
       * In the implementation `var`s and `mutable`s are used.
       * This is done intentionally to improve performance.
       */
      def apply(totalTrellisBuilder: TotalTrellisBuilder)(traces: TSignals): TSignals = {
        val toProcess: List[TSignal] =
          if isTrellisContactUsed then
            signalToTSignal(new Signal(TrellisContact, traces.map(tSignalToSignal))) :: traces //inners
          else
            traces

        val trackingTrellisBuilder = totalTrellisBuilder.newSingleStepBuilder

        @tailrec
        def processAllSignals(signalsToProcess: List[TSignal]): Unit =
          signalsToProcess match {
            case Nil =>
            case trace :: tail =>
              if stopContacts.contains(tSignalToSignal(trace).contact) then {
                // signals on contacts from stop-list are not processed.
                totalTrellisBuilder.saveStopSignal(trace)
              } else
                processSignal(trace, trackingTrellisBuilder)
              processAllSignals(tail)
          }

        processAllSignals(toProcess)

        trackingTrellisBuilder.toTrellisElement
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
    case class TrellisProducerLoopyTracking(trellisProducer: TrellisProducerSpeedyTracking,
                                            stopContacts: Set[Contact0]) extends TotalTrellisProducerTracking {

      def apply(context: Context, signal: Signal0): TrellisElementTracking = {
        val totalTrellisBuilderTracking = newTotalTrellisBuilder(trellisProducer.runtimeSystemForTrellisProcessing.runtimeSystem, context)
        val trellisProducer1 = trellisProducer(totalTrellisBuilderTracking)(_)
        try {
          def from(t0: TSignals): LazyList[TSignals] =
            t0 #:: from(trellisProducer1(t0))

          val finalTrellisElement = from(List(signalToTSignal(signal))).
            find { traces =>
            traces.forall(trace => stopContacts.contains(tSignalToSignal(trace).contact))
          }.get
          //WONTFIX: put final trellis on TrellisContact. This is impractical in the current architecture.
          // as it is intended only for debug purposes, thus we don't implement it not to sacrifice performance.
          (totalTrellisBuilderTracking.currentState, totalTrellisBuilderTracking.stopSignals ++ finalTrellisElement)
        } catch {
          case e: Exception =>
            throw new RuntimeException(
              s"Exception ${e.getClass.getSimpleName} during trellis rendering starting with '$signal'. " +
                s"Context value before processing:\n" + context.mkString("\n"), e)
        }
      }
    }

    /** Converts a runtime system to Trellis producer. */
    def rsToTrellisProducer(runtimeSystem: RuntimeSystem): TotalTrellisProducerTracking = {
      val rsftp = new RuntimeSystemForTrellisProcessingTracking(runtimeSystem)
      val step = TrellisProducerSpeedyTracking(rsftp)
      val loopy = TrellisProducerLoopyTracking(step, runtimeSystem.stopContacts)
      loopy
    }

  }

  val signalProcessing:SignalProcessing0

  implicit class RichRuntimeSystem(runtimeSystem: RuntimeSystem) {
    /** Converts the runtime system to a RuntimeComponentHeavy that does all inner processing in a single outer step. */
    def toTotalTrellisProducer: signalProcessing.TotalTrellisProducerTracking = //: TotalTrellisProducer
      signalProcessing.rsToTrellisProducer(runtimeSystem)

    //runtimeSystemToTotalTrellisProducerConverter(runtimeSystem)
    //    /** Converts the runtime system to a RuntimeComponentHeavy that does all inner processing in a single outer step. */
    //    def toTotalTrellisProducerTracking: TotalTrellisProducerTracking = {
    //      val rsftp = new RuntimeSystemForTrellisProcessingTracking(runtimeSystem)
    //      val step = TrellisProducerSpeedyTracking(rsftp)
    //      val loopy = TrellisProducerLoopyTracking(step, runtimeSystem.stopContacts)
    //      loopy
    //    }
  }

}
/** TODO replaceable mode of signal processing.*/
trait SignalProcessingTrackingApi extends SignalProcessingApi0 {
  /** Implementation of SignalProcessing with tracking parent signals in Trace class.*/
  trait SignalProcessingTracking extends SignalProcessing0 {
    type TSignal = Trace

    def tSignalToSignal(s: TSignal): Signal0 = s.signal

    def signalToTSignal(s: Signal0): TSignal = new Trace(s)

    class TrackingTrellisBuilder(
                                  newTraces: mutable.ListBuffer[TSignal] = mutable.ListBuffer[TSignal](),
                                  totalTrellisBuilderTracking: TotalTrellisBuilder
                                  ) extends TrellisBuilder {


      def addSignals(trace: TSignal, proc: RuntimeComponent, signals: SignalCollection[Signal0]): Unit = {
        val procs = proc :: trace.processorsReversed
        signals match {
          case Nil => totalTrellisBuilderTracking.saveTerminatedSignal(Trace(trace.signalsReversed, procs))
          case _ => newTraces ++= signals.iterator.map(s => Trace(s :: trace.signalsReversed, procs))
        }
      }

      override def addException(trace: TSignal, proc: RuntimeComponent, exception: Throwable): Unit = {
        totalTrellisBuilderTracking.addException(trace, proc, exception)
      }

      override def toTrellisElement: TSignals = newTraces.toList

      override def currentState: Context = totalTrellisBuilderTracking.currentState

      override def currentState_=(ctx: Context): Unit = {
        totalTrellisBuilderTracking.currentState = ctx
      }
    }

    def newTotalTrellisBuilder(runtimeSystem: RuntimeSystem, context: Context): TotalTrellisBuilder = new TotalTrellisBuilderTracking(runtimeSystem, context)

    class TotalTrellisBuilderTracking(runtimeSystem: RuntimeSystem, initialState: Context) extends TotalTrellisBuilder {
      val lostTraces: mutable.ListBuffer[TSignal] = mutable.ListBuffer[TSignal]()
      val stopSignalsBuilder: mutable.ListBuffer[TSignal] = mutable.ListBuffer[TSignal]()

      /** The list of signals on stop contacts. */
      def stopSignals: TSignals = stopSignalsBuilder.toList

      var currentState: Context = initialState

      /** Saves the signal that is the last one in a trace. It didn't produce output. */
      override def saveTerminatedSignal(signal: TSignal): Unit = lostTraces += signal

      /** Save the signal that appeared on the stop-contact. */
      override def saveStopSignal(signal: TSignal): Unit = stopSignalsBuilder += signal

      def newSingleStepBuilder: TrellisBuilder = new TrackingTrellisBuilder(mutable.ListBuffer[TSignal](), this)

      override def addException(trace: TSignal, proc: RuntimeComponent, exception: Throwable): Unit = {
        currentState = runtimeSystem.unhandledExceptionHandler(exception, proc.name, tSignalToSignal(trace), currentState)
      }
    }

  }

//  object SignalProcessingTracking extends  SignalProcessingTracking
  val signalProcessing:SignalProcessingTracking = new SignalProcessingTracking {}

}
trait SignalProcessingSimpleApi extends SignalProcessingApi0 {
  /** SignalProcessing without tracking.*/
  trait SignalProcessingSimple extends SignalProcessing0 {
    type TSignal = Signal0

    def tSignalToSignal(s: TSignal): Signal0 = s

    def signalToTSignal(s: Signal0): TSignal = s

    class TrackingTrellisBuilder(
                                  newTraces: mutable.ListBuffer[TSignal] = mutable.ListBuffer[TSignal](),
                                  totalTrellisBuilderTracking: TotalTrellisBuilder
                                  ) extends TrellisBuilder {


      def addSignals(trace: TSignal, proc: RuntimeComponent, signals: SignalCollection[Signal0]): Unit = {
        signals match {
          case Nil => totalTrellisBuilderTracking.saveTerminatedSignal(trace)
          case _ => newTraces ++= signals.iterator.map(signalToTSignal)
        }
      }

      override def addException(trace: TSignal, proc: RuntimeComponent, exception: Throwable): Unit = {
        totalTrellisBuilderTracking.addException(trace, proc, exception)
      }

      override def toTrellisElement: TSignals = newTraces.toList

      override def currentState: Context = totalTrellisBuilderTracking.currentState

      override def currentState_=(ctx: Context): Unit = {
        totalTrellisBuilderTracking.currentState = ctx
      }
    }

    def newTotalTrellisBuilder(runtimeSystem: RuntimeSystem, context: Context): TotalTrellisBuilder = new TotalTrellisBuilderTracking(runtimeSystem, context)

    class TotalTrellisBuilderTracking(runtimeSystem: RuntimeSystem, initialState: Context) extends TotalTrellisBuilder {
      val lostTraces: mutable.ListBuffer[TSignal] = mutable.ListBuffer[TSignal]()
      val stopSignalsBuilder: mutable.ListBuffer[TSignal] = mutable.ListBuffer[TSignal]()

      /** The list of signals on stop contacts. */
      def stopSignals: TSignals = stopSignalsBuilder.toList

      /** Save the signal that appeared on the stop-contact. */
      override def saveStopSignal(signal: TSignal): Unit = stopSignalsBuilder += signal

      var currentState: Context = initialState

      /** Saves the signal that is the last one in a trace. It didn't produce output. */
      override def saveTerminatedSignal(signal: TSignal): Unit = lostTraces += signal


      def newSingleStepBuilder: TrellisBuilder = new TrackingTrellisBuilder(mutable.ListBuffer[TSignal](), this)

      override def addException(trace: TSignal, proc: RuntimeComponent, exception: Throwable): Unit = {
        currentState = runtimeSystem.unhandledExceptionHandler(exception, "", trace, currentState)
      }
    }

  }

  object SignalProcessingSimple extends SignalProcessingSimple

  val signalProcessing:SignalProcessingSimple = new SignalProcessingSimple{}

}

/**
 * end-user API for trellis producers
 * @author zhizhelev, 25.03.15.
 */
trait SignalProcessingDsl
  extends TrellisApi
  with SignalProcessingApi0
  with SignalProcessingSimpleApi
