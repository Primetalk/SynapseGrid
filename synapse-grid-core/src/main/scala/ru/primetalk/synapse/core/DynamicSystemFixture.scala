///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 *
 * Created: 21.08.13, zhizhelev
 */
package ru.primetalk.synapse.core

import ru.primetalk.synapse.core.components.DynamicSystem

/** A class that allows to use Dynamic system in a more comfortable way.
  * One can send any data on any input of the dynamic system and
  * the results are kept in output buffer.
  * Occasionally one may read output signals (clearing them out if neccessary).
  */
class DynamicSystemFixture(dynamicSystem:DynamicSystem) {
  private val outputBuffer = scala.collection.mutable.ListBuffer[Signal[_]]()
  def send[T](input:Contact[T])(data:T) = {
    val inputSignal = Signal(input, data)
    val outputSignals = dynamicSystem.receive(inputSignal)
    outputBuffer ++= outputSignals
    this
  }
  def clear() {
    outputBuffer.clear()
  }
  def read[T](output:Contact[T]):List[T] =
    outputBuffer.toList.get(output)

  /** Removes signals that corresponds to the given contact
   * @return data from removed signals */
  def remove[T](output:Contact[T]):List[T] = {
    val (res, rest) = outputBuffer.toList.partition(output)
    outputBuffer.clear()
    outputBuffer ++= rest
    res.map(_.data)
  }

}
