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

import java.util.concurrent.atomic.AtomicBoolean

/** A variable with synchronized access*/
class SafeVariable[T](initialValue:T) {
  private var value = initialValue

  /** Public lock for explicit locking by ComputationalGraph.*/
  private
  val lock = new AtomicBoolean(true)
  /** Private lock. Can be avoided indeed.*/
  private def locked[T2](body: => T2): T2 =
    this.synchronized{
      body
    }

  def update(u:T=>T) {
    locked{
      value = u(value)
    }
  }
  def update2[T2](u:T=>(T, T2)):T2 =
    locked{
      val (newValue, result) = u(value)
      value = newValue
      result
    }

  def isLockAvailable =
    lock.get()

  def acquire() = lock.compareAndSet(true, false)
  def release() = lock.compareAndSet(false, true)
  def get:T = locked{ value }
  override def toString = "SV("+get+")"
}
