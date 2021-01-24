package ru.primetalk.synapse.core.ext

import ru.primetalk.synapse.core.components.Contact0
import ru.primetalk.synapse.core.dsl.SignalsApi
import ru.primetalk.synapse.core.subsystems.StaticSystemApi


trait ContactsIndexExt extends SystemBuilderApi with SignalsApi with StaticSystemApi {

  /**
   * ContactsIndex is used mainly in Distributed systems for obtaining serializable versions of signals (SignalDist).
   * Every system has lazy val index.
   **/
  trait ContactsIndex {
    /** All contacts, available at this system's level.
      * This is a stable linear sequence of contacts. In different JVMs it should be the same
      */
    def contacts: Seq[Contact0]

    /** Contact id by Contact. */
    lazy val reversedContactsIndex = contacts.zipWithIndex.toMap[Contact0, Int]

    /** Contact should be from the current system. */
    def convertSignalToSignalDist(s: Signal0): SignalDist = {
      val id: Int = reversedContactsIndex.getOrElse(s.contact, throw new IllegalArgumentException(s"There is no contact ${s.contact} in the index."))
      val res = SignalDist(id, s.data0.asInstanceOf[java.lang.Object]) // the cast is required because type Any doesn't exist in runtime.
      //    println(s"cnt.1:$s -> $res")
      res
    }

    def convertSignalDistToSignal(s: SignalDist): Signal0 = {
      val c = contacts(s.contactId).asInstanceOf[Contact[AnyRef]]
      val res = Signal(c, s.data)
      //    println(s"cnt.2:$s -> $res")
      res
    }

    def apply(s: Signal0): SignalDist = convertSignalToSignalDist(s)

    def apply(s: SignalDist): Signal0 = convertSignalDistToSignal(s)
  }

//  /**
//   * DONE remove Indexed from every System.
//   * Only create component "IndexedSubsystem" like InnerSystem
//   **/
//  trait Indexed {
//    def index: ContactsIndex
//  }
//
  case class ContactsIndexImpl(contacts: Seq[Contact0]) extends ContactsIndex {
    {
      // in order not to pollute namespace we encapsulate val in curly braces
      val duplicates = contacts.map(_.name).groupBy(identity).filter(_._2.size > 1)
      // sometimes users define contacts with the same name. It works in a single-JVM environment.
      // However, it is hard to deliver in serializable environment
      require(duplicates.size == 0,
        "There are duplicated contact names: " +
          duplicates.map(p => p._1 + "(" + p._2.size + ")").mkString(", "))
    }
  }

  implicit object ContactsIndexExtensionId extends StaticSystemExtensionId[ContactsIndex]

  implicit class StaticSystemIndexed(s:StaticSystem){
    def index: ContactsIndex =
      s.extensionOpt(ContactsIndexExtensionId).getOrElse{
        val idx = ContactsIndexImpl(s.allContacts):ContactsIndex
        s.extend(idx)(ContactsIndexExtensionId)
        idx
      }
  }
}
