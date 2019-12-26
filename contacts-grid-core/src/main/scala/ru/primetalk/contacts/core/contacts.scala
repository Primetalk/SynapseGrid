package ru.primetalk.contacts.core

trait Contacts {
  // customizable contact. User might create specific contacts with different data inside.
  // For instance, for remoting it might be convenient to have contacts identified by integers.
  trait Contact {
    type Data
  }
}

trait NamedContacts extends Contacts {

  final class NamedContact[A <: String,B](val name: A) extends Product with Serializable with Contact {
    override type Data = B
    override def productArity: Int = 0
    override def productElement(n: Int): Any = 0
    override def canEqual(that: Any): Boolean = that.isInstanceOf[NamedContact[_, _]]
    override def equals(o: Any): Boolean = o match {
      case n: NamedContact[_,_] => n.name == name
      case _ => false
    }

    override def hashCode(): Int = name.hashCode()
  }

  object NamedContact {

    def apply[A <: String : ValueOf, B]: NamedContact[A,B] =
      new NamedContact[A, B](implicitly[ValueOf[A]].value)

    implicit def valueOfNamedContact[A <: String : ValueOf, B]: ValueOf[NamedContact[A, B]] =
      new ValueOf[NamedContact[A, B]](apply[A, B])
  }

}

/** These are needed for `map` and `flatMap` API/DSL.
 {{{
 val myContact = Contact[Int]
 myContact.map( _ + 1) .flatMap( 0 until _) -
 }}}
  each map/flatMap will create a component (?) and a contact.
*/
trait AutoContacts extends Contacts
