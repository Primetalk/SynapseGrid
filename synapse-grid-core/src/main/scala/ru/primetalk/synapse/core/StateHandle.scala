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

/**
 * Stateful elements of the system
 */
trait Stateful[State] extends Named {
  type StateType = State
  /**
   * The initial state of the element
   */
  val s0: State
}

/**
 * Permanent contacts store shared state that can be updated with stateful
 * links.
 */
class StateHandle[S](name: String, val s0: S) extends Contact[S](name, StateContact) with Stateful[S] {
  override def toString = "S(" + name + ")"
}
object StateHandle {
  def apply[S](name: String, s0: S) = new StateHandle(name, s0)

  def unapply(s: Any): Option[(String, _)] =
    s match {
      case stateHandle: StateHandle[_] => Some(stateHandle.name, stateHandle.s0)
      case _ => None
    }
}

