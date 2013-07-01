///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SinapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 * (2-clause BSD license) See LICENSE
 *
 * Created: 30.06.13, zhizhelev
 */
package ru.primetalk.sinapse

import java.io.{File, PrintWriter}

package object core {

  implicit class RichStaticSystem(system: StaticSystem) {
    def toDot = SystemRenderer(system)

    def toDynamicSystem = SignalProcessing.toDynamicSystem(system)
  }

  implicit class RichDynamicSystem(system: DynamicSystem) {
    def toTransducer[TInput, TOutput](input: Contact[TInput], output: Contact[TOutput]) = {
      data: TInput =>
        val inputSignal = Signal(input, data)
        val outputSignals = system.receive(inputSignal)
        outputSignals.collect {
          case Signal(`output`, outputData) => outputData.asInstanceOf[TOutput]
        }
    }

    def toMapTransducer[TInput, TOutput](input: Contact[TInput], output: Contact[TOutput]) = {
      data: TInput =>
        val inputSignal = Signal(input, data)
        val outputSignals = system.receive(inputSignal)
        val outputs = outputSignals.collect {
          case Signal(`output`, outputData) => outputData.asInstanceOf[TOutput]
        }
        if (outputs.length != 1)
          throw new IllegalStateException(s"Cannot convert output results $outputs from $output to List(data).")
        outputs.head
    }

  }

  implicit class WritableString(s: String) {
    def saveTo(filePath: String) {
      val wrt = new PrintWriter(new File(filePath), "UTF-8")
      try {
        wrt.print(s)
      } finally {
        wrt.close()
      }
    }
  }

  implicit def filenameToFile(filename: String): File = new File(filename)

}
