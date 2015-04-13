package ru.primetalk.synapse.core.components

/**
 * @author zhizhelev, 13.04.15.
 */
object StateHandle {
   def apply[S](name: String, s0: S) = new StateHandle(name, s0)

   def unapply(s: Any): Option[(String, _)] =
     s match {
       case stateHandle: StateHandle[_] => Some(stateHandle.name, stateHandle.s0)
       case _ => None
     }
 }

/**
  * Permanent contacts store shared state that can be updated with stateful
  * links.
  */
class StateHandle[S](name: String, val s0: S) extends Contact[S](name) with Stateful[S] {
   override def toString = "S(" + name + ")"
 }