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

  def contact[T](name:String) = new Contact[T](name)

  /**
   * Extractor of contacts' data from result.
   */
  implicit class ContactExtractor[T](c: Contact[T]) {

    def signal(d: T) = Signal(c, d)
    def createSignal(d: T) = Signal(c, d)

    def createSignals(ds: T*): List[Signal[T]] = ds.map(Signal(c, _)).toList

    def get(signals: List[Signal[_]]) :List[T]= {
      val C = c
      signals.collect{case Signal(C, data) => data.asInstanceOf[T]}
    }
    def filterFunction = (signals: List[Signal[_]]) ⇒ signals.filter(_._1 == c).map(_.asInstanceOf[Signal[T]])

    def filterNotFunction = (signals: List[Signal[_]]) ⇒ signals.filterNot(_._1 == c)
  }

  implicit class RichSignalList(signals: List[Signal[_]]){
    /** Divides the list of signals. The first part will contain signals on the given contact.
      * the second — the rest signals.*/
    def partition[T](c:Contact[T]) :(List[Signal[T]], List[Signal[_]]) =
      signals.
        partition(_.contact == c).
        asInstanceOf[(List[Signal[T]], List[Signal[_]])]

    def get[T](`c`:Contact[T]) :List[T]=
      signals.
        collect{
          case Signal(`c`, data) =>
            data.asInstanceOf[T]
      }


  }
  implicit def pairToSignal[T](p: (Contact[T], T)) = Signal(p._1, p._2)

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

  type TrellisElement = (Context, List[Signal[_]])

  type ContextUpdater = List[(Contact[_], _)]

  /** The most general processing element.
    * Is very similar to the most generic link — StateFlatMap. */
  type RuntimeComponent = (Context, Signal[_]) => (ContextUpdater, List[Signal[_]])
  type RuntimeComponentHeavy = (Context, Signal[_]) => TrellisElement
  /** The simplest signal processor. Corresponds to FlatMap.*/
  type SimpleSignalProcessor = Signal[_] => List[Signal[_]]

  type TrellisElementUpdater = (ContextUpdater, List[Signal[_]])
  def updateTrellisElement(te:TrellisElement, upd:TrellisElementUpdater):TrellisElement =
    ((te._1 /: upd._1.reverse)((ctx, u) => ctx + u) , upd._2)

	/** A function that makes single(?) step over time. */
	type TrellisProducer = TrellisElement => TrellisElement

  type ContactToSubscribersMap = Map[Contact[_], List[RuntimeComponentHeavy]]

  implicit class RichRuntimeSystem(runtimeSystem:RuntimeSystem){
    /** Converts the runtime system to a RuntimeComponentHeavy that does all inner processing in a single outer step.*/
    def toRuntimeComponentHeavy = {
      val step = TrellisProducerSpeedy(runtimeSystem)
      TrellisProducerLoopy(step, runtimeSystem.stopContacts):RuntimeComponentHeavy
    }
//    def toSingleStepTrellisProducer = {
//      val step = TrellisProducerSpeedy(runtimeSystem)
//      TrellisProducerLoopy(step, runtimeSystem.stopContacts):RuntimeComponentHeavy
//    }
  }
}
