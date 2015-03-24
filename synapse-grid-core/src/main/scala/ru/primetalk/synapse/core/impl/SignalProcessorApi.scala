package ru.primetalk.synapse.core.impl

import ru.primetalk.synapse.core.{Signal, Contact}

/**
 * API for using Signal processors.
 * @author zhizhelev, 25.03.15.
 */
trait SignalProcessorApi {
  /** The simplest signal processor. Corresponds to FlatMap. */
  type SimpleSignalProcessor = Signal[_] => List[Signal[_]]

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
        if (outputs.length != 1)
          throw new IllegalStateException(s"Cannot convert output results $outputs from $output to List(data).")
        outputs.head
    }

  }


}
