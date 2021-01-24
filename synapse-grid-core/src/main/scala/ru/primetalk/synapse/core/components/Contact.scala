package ru.primetalk.synapse.core.components

/**
 * @author zhizhelev, 13.04.15.
 */
object Contact:
  def unapply(contact: Contact0): Option[String] =
    Some(contact.name)

sealed abstract class Contact0 extends Named with Serializable

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
class Contact[T](name1: String | Null = null) extends Contact0:
  val name: String =
    if name1.eq(null) then
      getClass.getSimpleName.replace("$", "")
    else
      name1

  override def toString: String = 
    "C(" + name + ")"
end Contact

trait StateHandle0 extends Contact0:
  def init: Any

/**
 * Permanent contacts store shared state that can be updated with stateful
 * links.
 */
class StateHandle[S](name: String, val s0: S) extends Contact[S](name) with Stateful[S] with StateHandle0:
  override def toString = "S(" + name + ")"
  def init = s0
end StateHandle

/**
 * @author zhizhelev, 13.04.15.
 */
object StateHandle:
   def unapply(s: Any): Option[(String, _)] =
     s match {
       case stateHandle: StateHandle[_] => Some(stateHandle.name, stateHandle.s0)
       case _ => None
     }
