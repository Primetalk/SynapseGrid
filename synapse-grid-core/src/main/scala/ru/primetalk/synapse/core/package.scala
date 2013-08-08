///////////////////////////////////////////////////////////////
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
 * Created: 30.06.13, zhizhelev
 */
package ru.primetalk.synapse

import java.io.{File, PrintWriter}
import scala.language.implicitConversions
import scala.language.reflectiveCalls

package object core {

  implicit def toStaticSystem(a:{ def toStaticSystem:StaticSystem }):StaticSystem = {
    a.toStaticSystem
  }
  implicit class RichStaticSystem(system: StaticSystem) {
    def toDot = SystemRenderer.staticSystem2ToDot(system)
    def toDotAtLevel(level:Int = 0) = SystemRenderer.staticSystem2ToDot(system, level = level)
    def toDynamicSystem = SystemConverting.toDynamicSystem(List(),system)
  }
  implicit class RichSystemBuilder(systemBuilder: BasicSystemBuilder)
    extends RichStaticSystem(systemBuilder.toStaticSystem){
    def system = systemBuilder.toStaticSystem
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

  /** The context for system is a map from state handles to values.*/
  type Context = Map[Contact[_], _]

  /** The most general processing element.
    * Is very similar to the most generic link — StateFlatMap. */
  type RuntimeComponent = (Context, Signal[_]) => (Context, List[Signal[_]])
  /** The simplest signal processor. Corresponds to FlatMap.*/
  type SimpleSignalProcessor = Signal[_] => List[Signal[_]]

  type TrellisElement = (Context, List[Signal[_]])
	/** A function that makes single(?) step over time. */
	type TrellisProducer = TrellisElement => TrellisElement

  type ContactToSubscribersMap = Map[Contact[_], List[RuntimeComponent]]

  implicit class RichRuntimeSystem(runtimeSystem:RuntimeSystem){
    def toRuntimeComponent = {
      val step = TrellisProducerSpeedy(runtimeSystem)
      TrellisProducerLoopy(step, runtimeSystem.stopContacts):RuntimeComponent
    }
  }
}
