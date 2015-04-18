package ru.primetalk.synapse.core.runtime

import ru.primetalk.synapse.core.components.SignalsApi
import ru.primetalk.synapse.core.dsl.{ContactsIndexExt, ExceptionHandlingExt}

/**
 * @author zhizhelev, 05.04.15.
 */
trait RuntimeSystemApi
  extends SignalsApi
  with ExceptionHandlingExt
  with RuntimeComponentApi with TrellisApi with ContactsIndexExt {

  /** A dictionary of handlers for signals that appear on contacts.*/
  type ContactToSubscribersMap = Map[Contact[_], List[RuntimeComponent]]
  /** This contact is used to enable special simultaneous processing of signals.
    * For instance the contact can be used for debug purposes.
    * */
  object TrellisContact extends Contact[SignalCollection[Signal[_]]]
  /** A runtime system is a representation of the system that is
    * reorganized by Contacts and is ready for direct processing of TrellisElement. */
  case class RuntimeSystem(name: String,
                           signalProcessors: ContactToSubscribersMap,
                           stopContacts: Set[Contact[_]],
                           unhandledExceptionHandler: UnhandledProcessingExceptionHandler
                           = defaultUnhandledExceptionHandler
                            ) {
    lazy val contacts = signalProcessors.keySet
    lazy val isTrellisContactUsed = contacts.contains(TrellisContact)
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
                            index: ContactsIndex) extends Named with Component

  type RuntimeSystemToTotalTrellisProducerConverter = RuntimeSystem => TotalTrellisProducer

}
