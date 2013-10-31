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
 * Created: 22.09.13, zhizhelev
 */
package ru.primetalk.synapse.concurrent

import scala.concurrent.Lock

/** A variable with synchronized access*/
class SafeVariable[T](initialValue:T) {
  private var value = initialValue
  val lock = new Lock
  def update(u:T=>T) {
    this.synchronized{
      value = u(value)
    }
  }
  def update2[T2](u:T=>(T, T2)):T2 =
    this.synchronized{
      val (newValue, result) = u(value)
      value = newValue
      result
    }

  def get:T = this.synchronized{ value }
  override def toString = "SV("+value+"; "+lock.available+")"
}
