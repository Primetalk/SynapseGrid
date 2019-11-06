package ru.primetalk.contacts.core

import scala.annotation.implicitNotFound
import TypeSets._
import ru.primetalk.contacts
import ru.primetalk.contacts.core
import UniSets._

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

    def get[C<:Contact, Contacts<:TypeSet](s: S[Contacts], c: C)(implicit cInContacts: C BelongsTo Contacts): Option[c.Data] = unwrap(s) match {
      case (contact, data) if contact == c => Some(data.asInstanceOf[c.Data])
      case _ => None
    }

    def getIterable[C<:Contact, Contacts<:TypeSet](s: S[Contacts], c: C)(implicit cInContacts: C BelongsTo Contacts): Iterable[c.Data] = unwrap(s) match {
      case (contact, data) if contact == c => Iterable.single(data.asInstanceOf[c.Data])
      case _ => Iterable.empty
    }
//      if(scEqC == null) None else Some(sctEqCT(unwrap(s)._2))
  //  def wrap[Cont<:Contact, Contacts](c: Cont, data: Cont#Data)(implicit cInContacts: Cont BelongsTo Contacts): S[Contacts] { type C = Cont }

    def projection[Cont <: Contact, Contacts<:TypeSet, B<:TypeSet]
    (s: S[Contacts]{ type C = Cont }, b: B)(implicit ev: Cont BelongsTo B)
    : S[B] { type C = Cont }

//    def projection0[A <: TypeSet, Signal <: S[A]]
//    (s: Signal, b: Predef.Set[Contact]): Iterable[S[Predef.Set[Contact]] { type C = s.C }] =
//      runtimeBelongsTo(s.contact, b).map{ sb => wrap[s.C, Predef.Set[Contact]](s.contact, s.data)(sb) }

//    def projection00Contact[A <: TypeSet, B <: TypeSet]
//    (b: B)(s: S[A]): Iterable[S[B] { type C = Contact }] = {
//      val contact = s.contact.asInstanceOf[Contact]
//      belongsTo0(contact, b).map{ sb => wrap[Contact, B](contact, s.data)(sb) }
//    }
  }


//  class unwrapSignal[C <: UnionHelper0](val unionHelper: C) {
//    def apply(out: unionHelper.Out, signalOnContacts: SignalOnContacts[unionHelper.Out])(implicit ops: SignalOnContactsOps[SignalOnContacts])
//    : (Iterable[SignalOnContacts[A]], Iterable[SignalOnContacts[B]]) = {
//      //val (s: String, "") = ("", "")
//      val (a: A, b: B) = unionHelper.unwrap(out)
//      // TODO: extract BelongsTo from UnionHelper
////      (
////        ops.projection0(signalOnContacts, a),
////        ops.projection0(signalOnContacts, b)
////      )
//      ???
//    }
//    def apply(a: A, b: B, signalOnContacts: SignalOnContacts[unionHelper.Out])(implicit ops: SignalOnContactsOps[SignalOnContacts])
//    : (Iterable[SignalOnContacts[A]], Iterable[SignalOnContacts[B]]) = {
//        // TODO: extract BelongsTo from UnionHelper
////        (
////          ops.projection0(signalOnContacts, a),
////          ops.projection0(signalOnContacts, b)
////        )
//      ???
//    }
//  }
//  abstract class unwrapSignalTrait[A <: TypeSet, B <: TypeSet, C <: UnionHelper[A,B]](val unionHelper: C) {
//   def apply(signalOnContacts: SignalOnContacts[unionHelper.Out])(implicit ops: SignalOnContactsOps[SignalOnContacts])
//    : (Iterable[SignalOnContacts[A]], Iterable[SignalOnContacts[B]])
//  }
//  class unwrapSignal2[A <: TypeSet, B <: TypeSet, C <: UnionHelper[A, B]](a: A, b: B)(implicit unionHelper: C) {
//      def apply(signalOnContacts: SignalOnContacts[unionHelper.Out])(implicit ops: SignalOnContactsOps[SignalOnContacts]): (Iterable[SignalOnContacts[A]], Iterable[SignalOnContacts[B]]) = {
//        (
//          ops.projection0[unionHelper.Out, A, SignalOnContacts[unionHelper.Out]](signalOnContacts, a),
//          ops.projection0[unionHelper.Out, B, SignalOnContacts[unionHelper.Out]](signalOnContacts, b)
//        )
//      }
//
//  }

  class unwrapSignal2[A <: TypeSet, B <: TypeSet](a: A, b: B) {
    def apply[C <: UnionHelper[A, B]](implicit unionHelper: C): {
      def apply(signalOnContacts: SignalOnContacts[unionHelper.Out])(implicit ops: SignalOnContactsOps[SignalOnContacts])
      : (Iterable[SignalOnContacts[A]], Iterable[SignalOnContacts[B]])
    } = new {
      def apply(signalOnContacts: SignalOnContacts[unionHelper.Out])(implicit ops: SignalOnContactsOps[SignalOnContacts]): (Iterable[SignalOnContacts[A]], Iterable[SignalOnContacts[B]]) = {
        (
        ???, //  ops.projection0[unionHelper.Out, A, SignalOnContacts[unionHelper.Out]](signalOnContacts, a),
        ??? //  ops.projection0[unionHelper.Out, B, SignalOnContacts[unionHelper.Out]](signalOnContacts, b)
        )
      }
    }
  }
//  }, ) {
//
//    def apply(signalOnContacts: SignalOnContacts[unionHelper.Out])(implicit ops: SignalOnContactsOps[SignalOnContacts])
//    : (Iterable[SignalOnContacts[A]], Iterable[SignalOnContacts[B]]) = {
//      // TODO: extract BelongsTo from UnionHelper
////      (
////        ops.projection0(signalOnContacts, a),
////        ops.projection0(signalOnContacts, b)
////      )
//      ???
//    }
//  }

  // <:<
  @implicitNotFound("Couldn't prove that signal contacts is a subset of the other signal contacts")
  trait SignalContactsAreCompatibleOneWay[SA, SB] {
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

  implicit class MyContactOps[Cont <: Contact](val c: Cont) {
    def wrap[Contacts <: TypeSet]
    (data: c.type#Data)
    (implicit signalOnContactsOps: SignalOnContactsOps[SignalOnContacts], ev: c.type BelongsTo Contacts): SignalOnContacts[Contacts] { type C = c.type } =
    ??? //  signalOnContactsOps.wrap[c.type, Contacts](c, data)
    def wrapper[Contacts <: TypeSet]
    (implicit signalOnContactsOps: SignalOnContactsOps[SignalOnContacts], ev: c.type BelongsTo Contacts): (c.type#Data) => SignalOnContacts[Contacts] { type C = c.type } =
      ??? // signalOnContactsOps.wrap[c.type, Contacts](c, _)
  }
  implicit class MyContactSetOps[Contacts <: TypeSet](a: Contacts)(implicit signalOnContactsOps: SignalOnContactsOps[SignalOnContacts]) {
    def wrap[Cont <: Contact]
    (c: Cont)(data: c.type#Data)
    (implicit ev: c.type BelongsTo Contacts): SignalOnContacts[Contacts] { type C = c.type } =
      ??? // signalOnContactsOps.wrap[c.type, Contacts](c, data)
  }
  implicit object MySignalOps extends SignalOnContactsOps[SignalOnContacts] {
//    override def wrap[
//      Cont <: Contact,
//      Contacts <: TypeSet
//    ](c: Cont, data: Cont#Data)
//     (implicit cInContacts: BelongsTo[Cont, Contacts])
//    : MySignalOnContacts[Contacts] {
//      type C = Cont
//    } = new MySignal[Cont, Contacts](c, data, cInContacts)

    def projection[Cont <: Contact, Contacts<:TypeSet, B<:TypeSet]
    (s: SignalOnContacts[Contacts]{ type C = Cont }, b: B)(implicit ev: Cont BelongsTo B): MySignalOnContacts[B]  { type C = Cont } = new MySignalOnContacts[B]{
      override def contactIsInContacts: Cont BelongsTo B = ev

      override type C = Cont

      override def contact: C = s.contact

      override def data: C#Data = s.data
    }

  }

  // Check if signals are compatible
  implicit def eqContactsEqSignals[A<:TypeSet, B<:TypeSet](implicit aIsSubsetOfB: A ⊂ B): SignalContactsAreCompatibleOneWay[SignalOnContacts[A], SignalOnContacts[B]] =
    (s: SignalOnContacts[A]) => ??? // new MySignal[s.C, B](s.contact, s.data, inferEBelongsToBIfEBelongsToASubset[s.C, A, B](s.contactIsInContacts, aIsSubsetOfB))

  // wraps a function into a component with a single input and single output.
  def lift[In <: Contact, Out <: Contact]
    (in: In, out: Out)
    (f: In#Data => Out#Data)
    (implicit signalOnContactsOps: SignalOnContactsOps[SignalOnContacts])
    : signalOnContactsOps.Set[In +: ∅] => Iterable[signalOnContactsOps.Set[Out +: ∅]] =
  {
    signalOnContactIn =>
      ??? //  signalOnContactsOps.getIterable(signalOnContactIn, in)
        //.map(f.andThen(signalOnContactsOps.wrap(out, _)))
  }

  def liftIterable[In <: Contact, Out <: Contact]
    (in: In, out: Out)
    (f: in.Data => Iterable[out.Data])
    (implicit signalOnContactsOps: SignalOnContactsOps[SignalOnContacts])
    : SignalOnContacts[In +: ∅] => Iterable[SignalOnContacts[Out +: ∅]] =
  {
    signalOnContactIn =>
      ??? //signalOnContactsOps.getIterable(signalOnContactIn, in)
       // .flatMap(f).map(signalOnContactsOps.wrap(out, _))
  }

  // wraps a single contact to be both input and output.
  def trivialLift[C <: Contact]
    (c: C)
    (implicit signalOnContactsOps: SignalOnContactsOps[SignalOnContacts])
    : SignalOnContacts[C +: ∅] => Iterable[SignalOnContacts[C +: ∅]] =
    Iterable.single

  // identity function to simply pass signal
  def identity[In <: Contact, Out <: Contact]
    (in: In, out: Out)
    (implicit
      ev: In#Data <:< out.Data,
      signalOnContactsOps: SignalOnContactsOps[SignalOnContacts]
    )
    : SignalOnContacts[In +: ∅] => Iterable[SignalOnContacts[Out +: ∅]]
  =
    signalOnContactIn =>
      ??? //signalOnContactsOps.getIterable(signalOnContactIn, in)
     //   .map(ev.andThen(signalOnContactsOps.wrap(out, _)))
}