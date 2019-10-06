package ru.primetalk.contacts.core

import scala.annotation.implicitNotFound
import TypeSets._
import ru.primetalk.contacts.core

trait Signals {

  trait Contact {
    type Data
  }

  // typeless signal, it might be used sometimes
  // for instance when declaring type classes
  trait SignalOnContacts0 {
    type C <: Contact
    def contact: C
    def data: C#Data
  }
  object SignalOnContacts0 {
    def unapply(s: SignalOnContacts0): (s.C, s.C#Data) = (s.contact, s.data)
  }
  implicit class SignalOps[Cont <: Contact, A<:TypeSet, S[_ <: TypeSet]<: SignalOnContacts0](val s: S[A] {type C = Cont}) {
    def project[B <: TypeSet](contacts: B)(implicit ev: s.C BelongsTo B, ops: SignalOnContactsOps[S]): S[B] { type C = Cont} =
      ops.projection[s.C, A, B](s, contacts)
  }

  // This signal has phantom type argument that means that it is compatible with
  // the given set of contacts. In particular, C should be in Contacts
  // This ERASES information about contact type.
  trait SignalOnContacts[Contacts <: TypeSet] extends SignalOnContacts0 {
    def contactIsInContacts: C BelongsTo Contacts
  }

  // typeclass for dealing with signals
  // restores type information based on the actual contact that is in the signal.
  trait SignalOnContactsOps[S[_<:TypeSet]<:SignalOnContacts0] {
    type Self = SignalOnContactsOps[S]
    type Set[A<:TypeSet] = S[A]
    def unwrap[Contacts<:TypeSet](s: S[Contacts]): (s.C, s.C#Data) = (s.contact, s.data)
    def get2[Contacts<:TypeSet](s: S[Contacts])(c: s.C): s.C#Data = unwrap(s)._2
    def get[C<:Contact, Contacts<:TypeSet](s: S[Contacts], c: C)(implicit cInContacts: C BelongsTo Contacts): Option[C#Data] = unwrap(s) match {
      case (contact, data) if contact == c => Some(data.asInstanceOf[C#Data])
      case _ => None
    }
    def getIterable[C<:Contact, Contacts<:TypeSet](s: S[Contacts], c: C)(implicit cInContacts: C BelongsTo Contacts): Iterable[C#Data] = unwrap(s) match {
      case (contact, data) if contact == c => Iterable.single(data.asInstanceOf[C#Data])
      case _ => Iterable.empty
    }
//      if(scEqC == null) None else Some(sctEqCT(unwrap(s)._2))
    def wrap[Cont<:Contact, Contacts<:TypeSet](c: Cont, data: Cont#Data)(implicit cInContacts: Cont BelongsTo Contacts): S[Contacts] { type C = Cont }
    def projection[Cont <: Contact, Contacts<:TypeSet, B<:TypeSet]
    (s: S[Contacts]{ type C = Cont }, b: B)(implicit ev: Cont BelongsTo B): S[B] { type C = Cont }
  }

  @implicitNotFound("Couldn't prove that signal contacts is a subset of the other signal contacts")
  trait SignalContactsAreSubset[SA <: SignalOnContacts0, SB <: SignalOnContacts0] {
    def apply(s: SA): SB
  }

}

trait MySignals extends Signals {

//  abstract class MyContact[T](val name: String) extends Contact {
//    override type Data = T
//  }
//
  sealed trait MySignalOnContacts[Contacts <: TypeSet] extends SignalOnContacts[Contacts]

  final class MySignal[Cont <: Contact, Contacts <: TypeSet] private[MySignals] (val c: Cont, val d: Cont#Data, ev: Cont BelongsTo Contacts) extends MySignalOnContacts[Contacts] {
    override type C = Cont

    override def contact: C = c

    override def contactIsInContacts: BelongsTo[Cont, Contacts] = ev

    override def data: C#Data = d

    override def equals(o: Any): Boolean = o match {
      case other: MySignal[_,_] => c == other.c && d == other.d
      case _ => false
    }

    override def hashCode(): Int = c.hashCode() + d.hashCode()

    override def toString: String = s"MySignal($c, $d)"
  }

  implicit class MyContactOps[Cont <: Contact](c: Cont) {
    def wrap[Contacts <: TypeSet]
    (data: Cont#Data)
    (implicit signalOnContactsOps: SignalOnContactsOps[MySignalOnContacts], ev: Cont BelongsTo Contacts): MySignalOnContacts[Contacts] { type C = Cont} =
      signalOnContactsOps.wrap[Cont, Contacts](c, data)
  }
  implicit object MySignalOps extends SignalOnContactsOps[MySignalOnContacts] {
    override def wrap[
      Cont <: Contact,
      Contacts <: TypeSet
    ](c: Cont, data: Cont#Data)
     (implicit cInContacts: BelongsTo[Cont, Contacts])
    : MySignalOnContacts[Contacts] {
      type C = Cont
    } = new MySignal[Cont, Contacts](c, data, cInContacts)
    def projection[Cont <: Contact, Contacts<:TypeSet, B<:TypeSet]
    (s: MySignalOnContacts[Contacts]{ type C = Cont }, b: B)(implicit ev: Cont BelongsTo B): MySignalOnContacts[B]  { type C = Cont } = new MySignalOnContacts[B]{
      override def contactIsInContacts: Cont BelongsTo B = ev

      override type C = Cont

      override def contact: C = s.contact

      override def data: C#Data = s.data
    }

  }

  // Check if signals are compatible
  implicit def eqContactsEqSignals[A<:TypeSet, B<:TypeSet](implicit aIsSubsetOfB: A IsSubset B): SignalContactsAreSubset[SignalOnContacts[A], SignalOnContacts[B]] =
    (s: SignalOnContacts[A]) => new MySignal[s.C, B](s.contact, s.data, inferEBelongsToBIfEBelongsToASubset[s.C, A, B](s.contactIsInContacts, aIsSubsetOfB))

  // wraps a function into a component with a single input and single output.
  def lift[In <: Contact, Out <: Contact]
  (in: In, out: Out)(f: In#Data => Out#Data)
  (implicit
   signalOnContactsOps: SignalOnContactsOps[MySignalOnContacts]
  ): signalOnContactsOps.Set[In +: ∅] => Iterable[signalOnContactsOps.Set[Out +: ∅]]
  =
    signalOnContactIn =>
      signalOnContactsOps.getIterable(signalOnContactIn, in)
        .map(f.andThen(signalOnContactsOps.wrap(out, _)))

  // wraps a single contact to be both input and output.
  def trivialLift[C <: Contact](c: C)(
    implicit signalOnContactsOps: SignalOnContactsOps[MySignalOnContacts]
  ): signalOnContactsOps.Set[C +: ∅] => Iterable[signalOnContactsOps.Set[C +: ∅]]
  =
    Iterable.single

  // identity function to simply pass signal
  def identity[In <: Contact, Out <: Contact]
  (in: In, out: Out)
  (implicit
    ev: In#Data <:< Out#Data,
    signalOnContactsOps: SignalOnContactsOps[MySignalOnContacts]
  ): signalOnContactsOps.Set[In +: ∅] => Iterable[signalOnContactsOps.Set[Out +: ∅]]
  =
    signalOnContactIn =>
      signalOnContactsOps.getIterable(signalOnContactIn, in)
        .map(ev.andThen(out.wrap(_)))
}