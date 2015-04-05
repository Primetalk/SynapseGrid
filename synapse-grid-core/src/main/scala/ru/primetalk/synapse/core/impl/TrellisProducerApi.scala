package ru.primetalk.synapse.core.impl

import ru.primetalk.synapse.core._
import ru.primetalk.synapse.core.components.DynamicSystem

/**
 * @author zhizhelev, 25.03.15.
 */
trait TrellisProducerApi {

  /** The context for system is a map from state handles to values. */
  type Context = Map[Contact[_], _]

  type TrellisElement = (Context, List[Signal[_]])

  type ContextUpdater = List[(Contact[_], _)]


  type TrellisElementUpdater = (ContextUpdater, List[Signal[_]])

  def updateTrellisElement(te: TrellisElement, upd: TrellisElementUpdater): TrellisElement =
    ((te._1 /: upd._1.reverse)((ctx, u) => ctx + u), upd._2)

  /** A function that makes single(?) step over time. */
  type TrellisProducer = TrellisElement => TrellisElement
  /** A function that takes a single signal on input and returns the last trellis element. */
  type TotalTrellisProducer = ((Context, Signal[_]) => TrellisElement)
  type ContactToSubscribersMap = Map[Contact[_], List[RuntimeComponent]]

  type RuntimeSystemToTotalTrellisProducerConverter = RuntimeSystem => TotalTrellisProducer

  implicit class RichRuntimeSystem(runtimeSystem: RuntimeSystem) {
    /** Converts the runtime system to a RuntimeComponentHeavy that does all inner processing in a single outer step. */
    def toTotalTrellisProducerOld: TotalTrellisProducer = {
      val rsftp = new RuntimeSystemForTrellisProcessing(runtimeSystem)
      val step = TrellisProducerSpeedy(rsftp)
      val loopy = TrellisProducerLoopy(step, runtimeSystem.stopContacts)
      loopy
    }
    /** Converts the runtime system to a RuntimeComponentHeavy that does all inner processing in a single outer step. */
    def toTotalTrellisProducer: TotalTrellisProducer = {
      import ru.primetalk.synapse.core.SignalProcessingSimple._
      val rsftp = new RuntimeSystemForTrellisProcessingTracking(runtimeSystem)
      val step = TrellisProducerSpeedyTracking(rsftp)
      val loopy = TrellisProducerLoopyTracking(step, runtimeSystem.stopContacts)
      loopy
    }
    //    /** Converts the runtime system to a RuntimeComponentHeavy that does all inner processing in a single outer step. */
    //    def toTotalTrellisProducerTracking: TotalTrellisProducerTracking = {
    //      val rsftp = new RuntimeSystemForTrellisProcessingTracking(runtimeSystem)
    //      val step = TrellisProducerSpeedyTracking(rsftp)
    //      val loopy = TrellisProducerLoopyTracking(step, runtimeSystem.stopContacts)
    //      loopy
    //    }
  }

  implicit class RichTotalTrellisProducer(ttp: TotalTrellisProducer) {
    /** Creates hidden state that will be maintained between different signals. */
    def toSimpleSignalProcessor(s0: Context): SimpleSignalProcessor = {
      var state: Map[Contact[_], _] = s0
      (signal: Signal[_]) => {
        val r = ttp(state, signal)
        state = r._1
        r._2
      }
    }
  }

  implicit class RichDynamicSystem(system: DynamicSystem) {

    def toTransducer[TInput, TOutput](input: Contact[TInput], output: Contact[TOutput]) =
      system.receive.toTransducer(input, output)

    def toMapTransducer[TInput, TOutput](input: Contact[TInput], output: Contact[TOutput]) =
      system.receive.toMapTransducer(input, output)

  }

}
