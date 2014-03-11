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
package ru.primetalk.synapse.core

/** An outer description of a system. */
trait Component extends Named {
  val inputContacts: Set[Contact[_]]
  val outputContacts: Set[Contact[_]]
}

//trait ComponentMeta
trait ComponentWithInternalStructure extends Component {
  def toStaticSystem: StaticSystem
}

object StaticSystem {
  type State = Map[Contact[_], Any]
}

case class StaticSystem(
                         /** A subset of contacts */
                         inputs: List[Contact[_]],
                         outputs: List[Contact[_]],
                         privateStateHandles: List[StateHandle[_]],
                         components: List[Component],
                         name: String) extends Named
with Component
with Stateful[Map[Contact[_], Any]]
with ComponentWithInternalStructure {
  lazy val inputContacts = inputs.toSet
  lazy val outputContacts = outputs.toSet
  /** Contacts that should be processed by SignalsProcessor. */
  lazy val processedContacts = inputContacts ++ components.flatMap(_.inputContacts)

  def isOutputContact(c: Contact[_]) = outputContacts.contains(c)

  lazy val s0 = (for {
    stateHandle ← privateStateHandles
  } yield (stateHandle, stateHandle.s0)).toMap[Contact[_], Any]

  lazy val staticSubsystems =
    components.collect {
      case component: ComponentWithInternalStructure => component.toStaticSystem
    }

  def toStaticSystem = this
}

/** Dynamic system. The state is kept inside the system. All complex logic
  * is implemented within receive function. */
case class DynamicSystem(
                          inputContacts: Set[Contact[_]],
                          outputContacts: Set[Contact[_]],
                          name: String,
                          receive: SimpleSignalProcessor) extends Named with Component {}


/** The system can be embedded into some other static system. It has state. */
case class InnerSystem[S](
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