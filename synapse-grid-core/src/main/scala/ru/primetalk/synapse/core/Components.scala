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

trait ContactsIndex {
  /** All contacts, available at this system's level.
    * This is a stable sequence of contacts
    * */
  def contacts: Seq[Contact[_]]

  lazy val reversedContactsIndex = contacts.toSeq.zipWithIndex.toMap[Contact[_], Int]

  /** Signal should be from the current system. */
  def convertSignalToSignalDist(s: Signal[_]): SignalDist = {
    val id: Int = reversedContactsIndex(s.contact)
    val res = SignalDist(id, s.data.asInstanceOf[java.lang.Object])
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

trait Indexed {
  def index: ContactsIndex
}

case class ContactsIndexImpl(contacts: Seq[Contact[_]]) extends ContactsIndex {
  {
    val duplicates = contacts.map(_.name).groupBy(identity).filter(_._2.size>1)
    require(duplicates.size == 0,
      "There are duplicated contact names: "+
        duplicates.map(p => p._1 + "(" + p._2.size + ")").mkString(", "))
  }
}

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
  lazy val processedContacts = inputContacts ++ components.flatMap(_.inputContacts)

  def isOutputContact(c: Contact[_]) = outputContacts.contains(c)

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
    * This is a stable sequence of contacts
    * */
  lazy val index: ContactsIndex = ContactsIndexImpl(
    (inputs.toSeq ++
      components.flatMap(_.inputContacts).toSeq ++
      components.flatMap(_.outputContacts).toSeq ++
      outputContacts.toSeq).toArray.toSeq.distinct
  )
}

/** Dynamic system. The state is kept inside the system. All complex logic
  * is implemented within receive function. */
case class DynamicSystem(
                          inputContacts: Set[Contact[_]],
                          outputContacts: Set[Contact[_]],
                          name: String,
                          receive: SimpleSignalProcessor,
                          index: ContactsIndex) extends Named with Component with Indexed {}


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