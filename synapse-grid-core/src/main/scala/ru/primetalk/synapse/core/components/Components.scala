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
 * Created: 28.06.13, zhizhelev
 */
package ru.primetalk.synapse.core.components


/** Transparent component whose internal structure can be represented as a StaticSystem.*/
trait ComponentWithInternalStructure extends Component0 with WithStaticSystem

/** The system that can be embedded into some other static system.
 * It has specially processed state:
 * @param s structure of the system
 * @param stateHandle the handle within parent system that holds internal system's state. The handle points to the map (stateHandle -> value)
 * @param sharedStateHandles a few state handles that are shared between the parent system and child.
 *                           During runtime processing current values from parent are copied to child state
 *                           before processing any signals and copied back afterwards.
 */
case class InnerSystemComponent(
 s: StaticSystem,
 stateHandle: StateHandle[Map[Contact0, Any]],
 sharedStateHandles: List[StateHandle0] = Nil,
) extends Component0 with ComponentWithInternalStructure {
 val inputContacts = s.inputContacts
 val outputContacts = s.outputContacts

 def name = s.name

 def toStaticSystem: StaticSystem = s
}

/**
 * Special component that atomically updates state. It doesn't have any output contact.
 */
case class StateUpdate[S, T2](
  from: Contact[T2],
  stateHandle: StateHandle[S],
  override val name: String,
  f: (S, T2) => S,
) // = (s : S, t : T2) => t)
  extends Component0 {
  lazy val inputContacts = Set(from)
  lazy val outputContacts = Set()
}

object StateUpdate {
  def replace[S, T2 <: S](s: S, t: T2) = t
}

/**
  * It's a low level uninterpretable black-box component that works directly on signals.
  * @param name of the component
  * @param inputContacts allowed input contacts
  * @param outputContacts output contacts
  * @param runtimeStatelessInterpreter interpreter that processes signal and returns
  */
case class BlackBoxStatelessComponent(
  override val name: String,
  override val inputContacts: Set[Contact0],
  override val outputContacts: Set[Contact0],
  runtimeStatelessInterpreter: SimpleSignalProcessor
) extends Component0

object BlackBoxStatelessComponent:
  given ComponentStatelessInterpreter[BlackBoxStatelessComponent] with
    def runtimeStatelessInterpreter(c: BlackBoxStatelessComponent): SimpleSignalProcessor = 
      c.runtimeStatelessInterpreter
