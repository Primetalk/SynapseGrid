///////////////////////////////////////////////////////////////
// СинаптическаяСеть
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
 * Created: 14.03.2013
 */
package ru.primetalk.synapse.core

import scala.language.implicitConversions

/**
 * Named is used to store graph specific information - label or name.
 */
trait Named {
  def name: String

  override def toString =
    getClass.getSimpleName + "(\"" + name + "\")"
}

sealed trait ContactStyle

case object NormalContact extends ContactStyle

case object AuxiliaryContact extends ContactStyle

case object DevNullContact extends ContactStyle

case object StateContact extends ContactStyle

/**
 * Basis point of connection of other elements.
 * If auxiliary then it is drawn on the graph as a simple little circle
 */
class Contact[T](name1: String = null, val contactStyle: ContactStyle = NormalContact) extends Named with Serializable {
  val name = if (name1 == null) getClass.getSimpleName.replaceAllLiterally("$", "") else name1

  override def toString = "C(" + name + ")"

  //  @throws(classOf[IOException])
  //  private def writeObject(out: ObjectOutputStream): Unit = {
  //    out.defaultWriteObject();
  //    out.writeString(name)
  //  }
  //
  //  @throws(classOf[IOException])
  //  private def readObject(in: ObjectInputStream): Unit = {
  //    in.defaultReadObject();
  //  }
}

object Contact {
  def unapply(c: Any): Option[(String, ContactStyle)] =
    c match {
      case contact: Contact[_] => Some(contact.name, contact.contactStyle)
      case _ => None
    }
}




