package ru.primetalk.synapse.core

import scala.annotation.tailrec
import scala.collection.mutable

/** A generic trait for different signal processing methods.
  * There are two descendants - a tracking one and a simple one.
  * The tracking signal processor keeps track of what signals were produced during processing.
  * The simple one do not save traces. So it requires less memory for processing.
  *
  * Keeping traces can greatly help with debugging. One may see which signals and processors have lead to 
  * the result.
  *
  * Having a common ancestor for both types of processing is advantageous because there will
  * be no code duplication among processors.
  */
trait SignalProcessing0 {
  /** The type that is used to represent a single value related to signal */
  type TSignal

  implicit def tSignalToSignal(s: TSignal): Signal[_]

  def signalToTSignal(s: Signal[_]): TSignal

  type TSignals = List[TSignal]
  type TrellisElement = (Context, TSignals)
  type TrellisElementTracking = (Context, TSignals)
  type TrellisProducerTracking = TotalTrellisBuilder => TSignals => TSignals
  type TotalTrellisProducerTracking = ((Context, Signal[_]) => TrellisElementTracking)

  def newTotalTrellisBuilder(context: Context): TotalTrellisBuilder

  trait TotalTrellisBuilder {
    var currentState: Context

    /** Saves the signal that is the last one in a trace. It didn't produce output. */
    def saveTerminatedSignal(signal: TSignal)

    /** Save the signal that appeared on the stop-contact. */
    def saveStopSignal(signal: TSignal)

    def newSingleStepBuilder: TrellisBuilder
  }

  /** Constructs trellis.
    * After running a proc over the trace signal, we
    * construct the next step of the trellis.
    * An instance of this class is constructed anew for every trellis step.
    *
    * */
  trait TrellisBuilder {
    def currentState: Context

    def currentState_=(ctx: Context)

    def addSignals(trace: TSignal, proc: RuntimeComponent, signals: List[Signal[_]])

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
      val signal = trace: Signal[_]
      val c = signal.contact
      for (proc ← signalProcessors(c)) {
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
              val s = trellisBuilder.currentState.asInstanceOf[Map[Contact[Any], Any]](sh)
              val (ns, signals) = f.asInstanceOf[Function2[Any, Signal[_], (Any, List[Signal[_]])]](s, signal)
              trellisBuilder.currentState = trellisBuilder.currentState.asInstanceOf[Map[Contact[Any], Any]].updated(sh, ns).asInstanceOf[Context]
              trellisBuilder.addSignals(trace, proc, signals)
          }
        } catch {
          case e: Throwable =>
            unhandledExceptionHandler(e, name, signal, trellisBuilder.currentState)
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
        if (isTrellisContactUsed)
          signalToTSignal(new Signal(TrellisContact, traces.map(tSignalToSignal))) :: traces //inners
        else
          traces

      val trackingTrellisBuilder = totalTrellisBuilder.newSingleStepBuilder

      @tailrec
      def processAllSignals(signalsToProcess: List[TSignal]): Unit =
        signalsToProcess match {
          case Nil =>
          case trace :: tail =>
            if (stopContacts.contains(tSignalToSignal(trace).contact)) {
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
                                          stopContacts: Set[Contact[_]]) extends TotalTrellisProducerTracking {

    def apply(context: Context, signal: Signal[_]): TrellisElementTracking = {
      val totalTrellisBuilderTracking = newTotalTrellisBuilder(context)
      val trellisProducer1 = trellisProducer(totalTrellisBuilderTracking)(_)
      try {
        def from(t0: TSignals): Stream[TSignals] =
          t0 #:: from(trellisProducer1(t0))

        val finalTrellisElement = from(List(signalToTSignal(signal))).
          find { traces =>
          traces.forall(trace => stopContacts.contains(tSignalToSignal(trace).contact))
        }.get
        //WONTFIX: put final trellis on TrellisContact. This is impractical in the current architecture.
        // as it is intended only for debug purposes, thus we don't implement it not to sacrifice performance.
        (totalTrellisBuilderTracking.currentState, finalTrellisElement)
      } catch {
        case e: Exception =>
          throw new RuntimeException(
            s"Exception ${e.getClass.getSimpleName} during trellis rendering starting with '$signal'. " +
              s"Context value before processing:\n" + context.mkString("\n"), e)
      }
    }
  }

  /** Converts a runtime system to Trellis producer. */
  def rsToTrellisProducer(runtimeSystem: RuntimeSystem): TotalTrellisProducerTracking

}

object SignalProcessingTracking extends SignalProcessing0 {
  type TSignal = Trace

  def tSignalToSignal(s: TSignal): Signal[_] = s.signal

  def signalToTSignal(s: Signal[_]): TSignal = new Trace(s)

  class TrackingTrellisBuilder(
                                newTraces: mutable.ListBuffer[TSignal] = mutable.ListBuffer[TSignal](),
                                totalTrellisBuilderTracking: TotalTrellisBuilder
                                ) extends TrellisBuilder {


    def addSignals(trace: TSignal, proc: RuntimeComponent, signals: List[Signal[_]]): Unit = {
      val procs = proc :: trace.processorsReversed
      signals match {
        case Nil => totalTrellisBuilderTracking.saveTerminatedSignal(Trace(trace.signalsReversed, procs))
        case _ => newTraces ++= signals.map(s => Trace(s :: trace.signalsReversed, procs))
      }
    }

    override def toTrellisElement: TSignals = newTraces.toList

    override def currentState: Context = totalTrellisBuilderTracking.currentState

    override def currentState_=(ctx: Context): Unit = {
      totalTrellisBuilderTracking.currentState = ctx
    }
  }

  def newTotalTrellisBuilder(context: Context): TotalTrellisBuilder = new TotalTrellisBuilderTracking(context)

  class TotalTrellisBuilderTracking(initialState: Context) extends TotalTrellisBuilder {
    val lostTraces: mutable.ListBuffer[TSignal] = mutable.ListBuffer[TSignal]()
    val stopSignals: mutable.ListBuffer[TSignal] = mutable.ListBuffer[TSignal]()
    var currentState: Context = initialState

    /** Saves the signal that is the last one in a trace. It didn't produce output. */
    override def saveTerminatedSignal(signal: TSignal): Unit = lostTraces += signal

    /** Save the signal that appeared on the stop-contact. */
    override def saveStopSignal(signal: TSignal): Unit = stopSignals += signal

    def newSingleStepBuilder: TrellisBuilder = new TrackingTrellisBuilder(mutable.ListBuffer[TSignal](), this)
  }

  def rsToTrellisProducer(runtimeSystem: RuntimeSystem): TotalTrellisProducerTracking = {
    /** Converts the runtime system to a RuntimeComponentHeavy that does all inner processing in a single outer step. */
    val rsftp = new RuntimeSystemForTrellisProcessingTracking(runtimeSystem)
    val step = TrellisProducerSpeedyTracking(rsftp)
    val loopy = TrellisProducerLoopyTracking(step, runtimeSystem.stopContacts)
    loopy
  }

}

object SignalProcessingSimple extends SignalProcessing0 {
  type TSignal = Signal[_]

  def tSignalToSignal(s: TSignal): Signal[_] = s

  def signalToTSignal(s: Signal[_]): TSignal = s

  class TrackingTrellisBuilder(
                                newTraces: mutable.ListBuffer[TSignal] = mutable.ListBuffer[TSignal](),
                                totalTrellisBuilderTracking: TotalTrellisBuilder
                                ) extends TrellisBuilder {


    def addSignals(trace: TSignal, proc: RuntimeComponent, signals: List[Signal[_]]): Unit = {
      signals match {
        case Nil => totalTrellisBuilderTracking.saveTerminatedSignal(trace)
        case _ => newTraces ++= signals.map(signalToTSignal)
      }
    }

    override def toTrellisElement: TSignals = newTraces.toList

    override def currentState: Context = totalTrellisBuilderTracking.currentState

    override def currentState_=(ctx: Context): Unit = {
      totalTrellisBuilderTracking.currentState = ctx
    }
  }

  def newTotalTrellisBuilder(context: Context): TotalTrellisBuilder = new TotalTrellisBuilderTracking(context)

  class TotalTrellisBuilderTracking(initialState: Context) extends TotalTrellisBuilder {
    val lostTraces: mutable.ListBuffer[TSignal] = mutable.ListBuffer[TSignal]()
    val stopSignals: mutable.ListBuffer[TSignal] = mutable.ListBuffer[TSignal]()
    var currentState: Context = initialState

    /** Saves the signal that is the last one in a trace. It didn't produce output. */
    override def saveTerminatedSignal(signal: TSignal): Unit = lostTraces += signal

    /** Save the signal that appeared on the stop-contact. */
    override def saveStopSignal(signal: TSignal): Unit = stopSignals += signal

    def newSingleStepBuilder: TrellisBuilder = new TrackingTrellisBuilder(mutable.ListBuffer[TSignal](), this)
  }

  def rsToTrellisProducer(runtimeSystem: RuntimeSystem): TotalTrellisProducerTracking = {
    /** Converts the runtime system to a RuntimeComponentHeavy that does all inner processing in a single outer step. */
    val rsftp = new RuntimeSystemForTrellisProcessingTracking(runtimeSystem)
    val step = TrellisProducerSpeedyTracking(rsftp)
    val loopy = TrellisProducerLoopyTracking(step, runtimeSystem.stopContacts)
    loopy
  }

}