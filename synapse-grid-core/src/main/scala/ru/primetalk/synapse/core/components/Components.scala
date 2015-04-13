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

/**
 * Named is used to store graph specific information - label or name.
 */
trait Named {
  def name: String

  override def toString =
    getClass.getSimpleName + "(\"" + name + "\")"
}

/**
 * Stateful elements of the system.
 */
trait Stateful[State] {
  type StateType = State
  /**
   * The initial state of the element.
   */
  val s0: State
}

/** An outer description of a system.
  * Actual description is deferred to descendants.
  * See also [[Link]]s
  */
trait Component extends Named {
  val inputContacts: Set[Contact[_]]
  val outputContacts: Set[Contact[_]]
}

/** A component that has single input and single output.
  */
trait TwoPoleComponent[T1, T2] extends Component {
  def from: Contact[T1]
  def to: Contact[T2]
  lazy val inputContacts : Set[Contact[_]] = Set(from)
  lazy val outputContacts : Set[Contact[_]] = Set(to)
}
/** Transparent component whose internal structure can be represented as a StaticSystem.*/
trait ComponentWithInternalStructure extends Component {
  /** The key method of synapse grid library. Returns a transparent representation
    * of the component in the form of system graph.*/
  def toStaticSystem: StaticSystem
}







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

                           /** main state handle that will hold private state of the subsystem. */
                           stateHandle: StateHandle[Map[Contact[_], Any]],

                           /** State handles to be shared with parent */
                           sharedStateHandles: List[StateHandle[_]] = Nil) extends Component with ComponentWithInternalStructure {
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
                               f: (S, T2) ⇒ S) // = (s : S, t : T2) ⇒ t)
  extends Component {
  lazy val inputContacts = Set(from): Set[Contact[_]]
  lazy val outputContacts = Set[Contact[_]]() //stateHolder)
}

object StateUpdate {
  def replace[S, T2 <: S](s: S, t: T2) = t
}