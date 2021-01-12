package ru.primetalk.synapse.core.components

/**
 * @author zhizhelev, 13.04.15.
 */
object Contact {
   def unapply(contact: Contact[_]): Option[String] = Some(contact.name)
 }

/**
  * Basis point of connection of other elements.
  * If auxiliary then it is drawn on the graph as a simple little circle.
  *
  * It order to improve performance the contact is compared by referential equality (#eq()).
  * That's why it is not a case class. However, this creates some inconvenience in serialization.
  *
  * NB: the contact is not very well serializable. After deserialization
  * we obtain a different instance of the contact. But in most cases the comparison is done by
  * referential equality (.eq) and thus it won't do well.
  *
  * In synapse-grid-akka there is a solution for Contact serializations.
  *
  * @see ru.primetalk.synapse.akka.ContactSerializer.
  *
  */
class Contact[T](name1: String = null) extends Named with Serializable {
   val name: String = if (name1 == null) getClass.getSimpleName.replace("$", "") else name1

   override def toString: String = "C(" + name + ")"
 }