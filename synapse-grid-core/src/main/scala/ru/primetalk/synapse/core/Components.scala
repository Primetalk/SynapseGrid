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

/** An outer description of a system.
  * Actual description is deferred to descendants.
  * See also [[ru.primetalk.synapse.core.Link]]s
  */
trait Component extends Named {
  val inputContacts: Set[Contact[_]]
  val outputContacts: Set[Contact[_]]
}

/** Transparent component whose internal structure can be represented as a StaticSystem.*/
trait ComponentWithInternalStructure extends Component {
  /** The key method of synapse grid library. Returns a transparent representation
    * of the component in the form of system graph.*/
  def toStaticSystem: StaticSystem
}

object StaticSystem {
  type State = Map[Contact[_], Any]
}

/**
 * ContactsIndex is used mainly in Distributed systems for obtaining serializable versions of signals (SignalDist).
 * Every system has lazy val index.
 * */
trait ContactsIndex {
  /** All contacts, available at this system's level.
    * This is a stable linear sequence of contacts. In different JVMs it should be the same
    */
  def contacts: Seq[Contact[_]]

  /** Contact id by Contact.*/
  lazy val reversedContactsIndex = contacts.toSeq.zipWithIndex.toMap[Contact[_], Int]

  /** Contact should be from the current system. */
  def convertSignalToSignalDist(s: Signal[_]): SignalDist = {
    val id: Int = reversedContactsIndex.getOrElse(s.contact, throw new IllegalArgumentException(s"There is no contact ${s.contact} in the index."))
    val res = SignalDist(id, s.data.asInstanceOf[java.lang.Object]) // the cast is required because type Any doesn't exist in runtime.
//    println(s"cnt.1:$s -> $res")
    res
  }

  def convertSignalDistToSignal(s: SignalDist): Signal[_] = {
    val c = contacts(s.contactId).asInstanceOf[Contact[AnyRef]]
    val res = Signal(c, s.data)
//    println(s"cnt.2:$s -> $res")
    res
  }

  def apply(s: Signal[_]): SignalDist = convertSignalToSignalDist(s)

  def apply(s: SignalDist): Signal[_] = convertSignalDistToSignal(s)
}

/**
 * TODO remove Indexed from every System.
 * Only create component "IndexedSubsystem" like InnerSystem
 * */
trait Indexed {
  def index: ContactsIndex
}

case class ContactsIndexImpl(contacts: Seq[Contact[_]]) extends ContactsIndex {
  { // in order not to pollute namespace we encapsulate val in curly braces
    val duplicates = contacts.map(_.name).groupBy(identity).filter(_._2.size>1)
    // sometimes users define contacts with the same name. It works in a single-JVM environment.
    // However, it is hard to deliver in serializable environment
    require(duplicates.size == 0,
      "There are duplicated contact names: "+
        duplicates.map(p => p._1 + "(" + p._2.size + ")").mkString(", "))
  }
}

/**
 * The core class for SynapseGrid. Contains an immutable description of a system.
 * @param inputs input contacts of the system. Within the system it is prohibited to send signals on them.
 * @param outputs output contacts of the system. Within the system it is prohibited to connect outgoing links
 *                to these contacts. This is due to the fact that the signals that come to output contacts
 *                are not processed within the system. They are delayed for processing by the outer system.
 * @param privateStateHandles state identifiers for variables available within the system.
 *                            The system itself is immutable and it is prohibited to save state somewhere in
 *                            closures or global vars (due to thread unsafety). Instead the system's internal
 *                            state is "provided" by runtime system in the form of map stateHandle->value.
 *                            updates of states can be done only in a purely functional way.
 *
 * @param components inner parts of the system - links, subsystems and other blocks. They have inputs and outputs.
 * @param name the system's name
 * @param unhandledExceptionHandler user-defined exception handler. It can recover from exception by
 *                                  returning repaired Context, log it or rethrow.
 */
case class StaticSystem( /** A subset of contacts */
                         inputs: List[Contact[_]],
                         outputs: List[Contact[_]],
                         privateStateHandles: List[StateHandle[_]],
                         components: List[Component],
                         name: String,
                         unhandledExceptionHandler:UnhandledProcessingExceptionHandler
                         = defaultUnhandledExceptionHandler
                         ) extends Named
with Component
with Stateful[Map[Contact[_], Any]]
with ComponentWithInternalStructure
with Indexed {
  lazy val inputContacts = inputs.toSet
  lazy val outputContacts = outputs.toSet
  /** Contacts that should be processed by SignalsProcessor. */
//  lazy val processedContacts = inputContacts ++ components.flatMap(_.inputContacts)

  def isOutputContact(c: Contact[_]) = outputContacts.contains(c)

  /** Initial state of the system.*/
  lazy val s0 = (for {
    stateHandle ← privateStateHandles
  } yield (stateHandle, stateHandle.s0)).toMap[Contact[_], Any]

  lazy val staticSubsystems =
    components.collect {
      case component: ComponentWithInternalStructure => component.toStaticSystem
    }

  def toStaticSystem =
    this

  /** All contacts, available at this system's level.
    * This is a stable sequence of contacts. TODO: move to a separate DistributedSubsystem.
    * This is a stable sequence of contacts. TODO: move to a separate DistributedSubsystem.
    * */
  lazy val index: ContactsIndex = ContactsIndexImpl(
    (inputs.toSeq ++
      components.flatMap(_.inputContacts).toSeq ++
      components.flatMap(_.outputContacts).toSeq ++
      outputContacts.toSeq).toArray.toSeq.distinct
  )
}

/** Dynamic system. The state is kept inside the system. All complex logic
  * is implemented within receive function.
  * Dynamic system can be added to StaticSystem as a simple component ("black box").
  * The processing of the dynamic system is done within a single step of
  * the outer system processor.
  */
case class DynamicSystem(
                          inputContacts: Set[Contact[_]],
                          outputContacts: Set[Contact[_]],
                          name: String,
                          receive: SimpleSignalProcessor,
                          index: ContactsIndex) extends Named with Component with Indexed {}


/** The system that can be embedded into some other static system.
  * It has specially processed state:
  * @param s structure of the system
  * @param stateHandle the handle within parent system that holds internal system's state.
  * @param sharedStateHandles a few state handles that are shared between the parent system and child.
  *                           During runtime processing current values from parent are copied to child state
  *                           before processing any signals and copied back afterwards.
  */
case class InnerSystem(
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