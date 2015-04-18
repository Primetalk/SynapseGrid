package ru.primetalk.synapse.core.runtime

import scala.collection.{GenTraversableOnce, GenTraversable}

/**
 * API for using Signal processors.
 * @author zhizhelev, 25.03.15.
 */
trait RichSimpleSignalProcessorApi extends TrellisApi{
  implicit class RichSimpleSignalProcessor(sp: SimpleSignalProcessor) {
    def toTransducer[TInput, TOutput](input: Contact[TInput], output: Contact[TOutput]) = {
      data: TInput =>
        val inputSignal = Signal(input, data)
        val outputSignals = sp(inputSignal)
        outputSignals.collect {
          case Signal(`output`, outputData) => outputData.asInstanceOf[TOutput]
        }
    }

    def toMapTransducer[TInput, TOutput](input: Contact[TInput], output: Contact[TOutput]) = {
      data: TInput =>
        val inputSignal = Signal(input, data)
        val outputSignals = sp(inputSignal)
        val outputs = outputSignals.collect {
          case Signal(`output`, outputData) => outputData.asInstanceOf[TOutput]
        }
        if (outputs.isEmpty)
          throw new IllegalStateException(s"Cannot convert empty output results $outputs from $output to List(data).")
        val result = outputs.head
        if (outputs.tail.nonEmpty)
          throw new IllegalStateException(s"Cannot convert multiple output results $outputs from $output to List(data).")
        result
    }

  }


}
