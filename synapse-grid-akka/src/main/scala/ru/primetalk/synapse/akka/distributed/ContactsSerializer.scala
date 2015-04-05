///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2014                                   //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2014.
 * All rights reserved.
 * Authors: A.Zhizhelev
 *
 * Created: 31.05.14, zhizhelev
 */
package ru.primetalk.synapse.akka.distributed

import akka.actor.{Extension, ExtensionId, ExtendedActorSystem}
import akka.serialization._
import ru.primetalk.synapse.core._
import ru.primetalk.synapse.akka.SpecialActorContacts._
import ru.primetalk.synapse.core.components.StaticSystem
import ru.primetalk.synapse.core.runtime.SubsystemSpecialContact


/** Contacts are serialized as long identifiers.
  * The identifiers are obtained from ActorSystem Extensions. */
class ContactsSerializer(system: ExtendedActorSystem) extends Serializer {

  //  private
  //  val javaSeriaizer = new akka.serialization.JavaSerializer(system)
  lazy val akkaSerialization = SerializationExtension(system)

  val contactsExtension: ContactsMapExtensionImpl = ContactsMapExtension(system)

  // This is whether "fromBinary" requires a "clazz" or not
  def includeManifest: Boolean = false

  def identifier = 43751

  private object Codes {
    val internalSignals = 1
    val signal = 2
    val contact = 3
  }

  def toBinary(obj: AnyRef): Array[Byte] = {
    //    val out = new ObjectOutputStream
    obj match {
      //      case InternalSignals(path, signals) =>
      //
      case Signal(c, data: AnyRef) =>
        val contactArr = toBinary(c)
        val serializer = akkaSerialization.findSerializerFor(data)
        val dataArr = serializer.toBinary(data)
        val res = contactArr ++ dataArr
        res
      case c: Contact[_] =>
        val id = contactsExtension.getContactId(c)
        Array[Byte](id.toByte, (id >> 8).toByte, (id >> 16).toByte, (id >> 24).toByte)
      case msg =>
        throw new IllegalArgumentException(s"Cannot serialize $msg")
    }
  }

  def fromBinary(bytes: Array[Byte],
                 clazz: Option[Class[_]]): AnyRef = {
    val id =
      bytes(0) +
        bytes(1) << 8 +
        bytes(2) << 16 +
        bytes(3) << 24
    val c = contactsExtension.getContact(id).asInstanceOf[Contact[Any]]
    //    val serializer = akkaSerialization.deserialize(bytes.drop(4))findSerializerFor(data)
    val data = akkaSerialization.deserialize(bytes.drop(4), classOf[java.lang.Object]) //serializer.fromBinary(bytes.drop(4))
    Signal[Any](c, data)
  }
}

case class ContactsIndex(system: StaticSystem) {
  private var n = 0L
  val (nToContact, contactToN) = {
    val arr = (Set(NonSignalWithSenderInput, SubsystemSpecialContact, CurrentTimeMsInput) ++
      system.allInputContacts).toArray
    val cn = arr.toSeq.zipWithIndex
    (arr, cn.toMap[Contact[_], Int])
  }
}

class ContactsMapExtensionImpl extends Extension {
  private
  var index: Option[ContactsIndex] = None

  def setSystem(system: StaticSystem) {
    index = Some(ContactsIndex(system))
  }

  def getContactId(c: Contact[_]): Int =
    index.get.contactToN(c)

  def getContact[T](id: Int): Contact[T] =
    index.get.nToContact(id).asInstanceOf[Contact[T]]
}

object ContactsMapExtension extends ExtensionId[ContactsMapExtensionImpl] {
  override def createExtension(system: ExtendedActorSystem) = new ContactsMapExtensionImpl
}