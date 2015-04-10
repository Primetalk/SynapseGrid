package ru.primetalk.synapse.core.runtime

import ru.primetalk.synapse.core.Contact




/**
 * end-user API for trellis producers
 * @author zhizhelev, 25.03.15.
 */
trait TrellisProducerApi extends TrellisProducerImpl
with TrellisApi
with RichSimpleSignalProcessorApi
with SignalProcessingApi0 {


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
      import SignalProcessingSimple._
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
      new RichSimpleSignalProcessor(system.receive).toTransducer(input, output)

    def toMapTransducer[TInput, TOutput](input: Contact[TInput], output: Contact[TOutput]) =
      new RichSimpleSignalProcessor(system.receive).toMapTransducer(input, output)

  }

}
