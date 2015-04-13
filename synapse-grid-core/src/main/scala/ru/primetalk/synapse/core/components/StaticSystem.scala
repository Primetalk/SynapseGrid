package ru.primetalk.synapse.core.components

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
 * extension methods:
 * unhandledExceptionHandler - user-defined exception handler. It can recover from exception by
 *                                  returning repaired Context, log it or rethrow.
 * index - ContactsIndex
 * styles - ContactsStyles
 */
case class StaticSystem( /** A subset of contacts */
                         inputs: List[Contact[_]],
                         outputs: List[Contact[_]],
                         privateStateHandles: List[StateHandle[_]],
                         components: List[Component],
                         name: String,
                         //                         unhandledExceptionHandler:UnhandledProcessingExceptionHandler
                         //                         = defaultUnhandledExceptionHandler,
                         extensions:Map[StaticSystemExtensionId[_], Any] = Map()
                         ) extends Component
with Named
with Stateful[Map[Contact[_], Any]]
with ComponentWithInternalStructure {
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
    * This is a stable sequence of contacts.
    * */
  def allContacts: Seq[Contact[_]] = (inputs.toSeq ++
    components.flatMap(_.inputContacts).toSeq ++
    components.flatMap(_.outputContacts).toSeq ++
    outputContacts.toSeq).toArray.toSeq.distinct
  //  lazy val index: ContactsIndex = ContactsIndexImpl(allContacts)

  def extend[T](ext:T)(implicit extId:StaticSystemExtensionId[T]) =
    copy(extensions = extensions.updated(extId, ext))

  def extensionOpt[T](implicit extId:StaticSystemExtensionId[T]):Option[T] =
    extensions.get(extId).asInstanceOf[Option[T]]
}

/**
 * @author zhizhelev, 13.04.15.
 */
object StaticSystem {
  type State = Map[Contact[_], Any]
}

/** ExtensionId for a StaticSystem extension. Every extension can be
  * installed only once on the same StaticSystem.
  *
  * The extension can contain some additional state for system processing.
  *
  * However, it is not recommended to add mutable state to otherwise immutable StaticSystem.
  */
trait StaticSystemExtensionId[+T]
