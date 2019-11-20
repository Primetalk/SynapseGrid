package ru.primetalk.contacts.core

import scala.annotation.implicitNotFound
import UniSets._

trait Signals {

  // customizable contact. User might create specific contacts with different data inside.
  // For instance, for remoting it might be convenient to have contacts identified by integers.
  trait Contact {
    type Data
  }

  // typeless signal, it might be used sometimes
  // for instance when declaring type classes
  trait SignalOnContact {
    type C <: Contact
    val contact: C
    val data: C#Data

    override def equals(o: Any): Boolean = o match {
      case other: SignalOnContact => contact == other.contact && data == other.data
      case _ => false
    }
    override def hashCode(): Int = contact.hashCode() + (if(data == null) 0 else data.hashCode())
  }
  object SignalOnContact {
    def unapply(s: SignalOnContact): (s.C, s.C#Data) = (s.contact, s.data)
    def apply[Cont <: Contact](c: Cont)(d: c.Data): SignalOnContact{ type C = c.type } = new SignalOnContact{
      type C = c.type
      val contact: c.type = c
      val data: contact.Data = d
    }
  }
//  implicit class SignalOps[Cont <: Contact, A<:UniSet, S[_ <: UniSet]<: SignalOnContacts0](val s: S[A] {type C = Cont}) {
//    def project[B <: UniSet](contacts: B)(implicit ev: s.C BelongsTo B, ops: SignalOnContactsOps[S]): S[B] { type C = Cont} =
//      ops.projection[s.C, A, B](s, contacts)
//  }

  // This signal has phantom type argument that means that it is compatible with
  // the given set of contacts. In particular, C should be in Contacts
  // This effectively ERASES information about the contact type.
  sealed trait Signal[Contacts <: UniSet] {
    // the original signal.
    // During construction we guarantee that it's contact is in the set.
    val signal1: SignalOnContact

    // having this method and be able to return a real value from it means that we have
    // evidence that this signal is indeed on one of the contact from the set.
//    def contactIsInContacts: C BelongsTo Contacts

    def safeUnwrap[C<:Contact](c: C)(implicit c1: Cardinality1[c.type, Contacts]): c.Data =
      signal1.data.asInstanceOf[c.Data]

    def safeUnwrap2[C<:Contact](implicit c1: Cardinality1[C, Contacts]): C#Data =
      signal1.data.asInstanceOf[C#Data]
//
//    def safeUnwrap3[Up<:Contact](implicit c1: EachElementIsSubtype[Up, Contacts]): Up#Data =
//      signal1.data.asInstanceOf[Up#Data]

    def unwrap[C<:Contact](c: C): Option[C#Data] =
      if(signal1.contact == c)
        Option(signal1.data.asInstanceOf[C#Data])
      else
        None

    def projection0[Cs <: UniSet](implicit r: Render[Contact, Cs]): Option[Signal[Cs]] =
      if(r.elements.contains(signal1.contact))
        Some(this.asInstanceOf[Signal[Cs]])
      else
        None

    def projection0Either[Cs <: UniSet](implicit r: Render[Contact, Cs]): Either[Signal[Subtract[Contacts, Cs]], Signal[Cs]] =
      if(r.elements.contains(signal1.contact))
        Right(this.asInstanceOf[Signal[Cs]])
      else
        Left(this.asInstanceOf[Signal[Subtract[Contacts, Cs]]])

    def cProjection[Cs <: UniSet](implicit s: IsSubSetOf[Contacts, Cs]): Signal[Cs] =
      this.asInstanceOf[Signal[Cs]]


    override def equals(o: Any): Boolean = o match {
      case other: Signal[_] => signal1 == other.signal1
      case _ => false
    }

    override def hashCode(): Int = signal1.hashCode()

  }

  type SignalProcessor[A<:UniSet,B<:UniSet] = Signal[A] => Iterable[Signal[B]]
  type >>[A<:UniSet,B<:UniSet] = Signal[A] => Iterable[Signal[B]]
  type Si[A <: Contact] = Singleton[A]

  def signal[Cs <: UniSet](s: SignalOnContact)(implicit ccs: BelongsTo[s.C, Cs]): Signal[Cs] = new Signal[Cs] {
    override val signal1: SignalOnContact = s
  }


//  // typeclass for dealing with signals
//  // restores type information based on the actual contact that is in the signal.
//  trait SignalOnContactsOps[S[_<:UniSet] <: Signal[_<:UniSet]] { self =>
//
//    type Self = SignalOnContactsOps[S]
//
//    type Set[A<:UniSet] = S[A]
//
//    def safeUnwrap[Cs <: UniSet, C<:Contact](s: S[Cs], c: C)(implicit c1: Cardinality1[c.type, Cs]): (c.type, c.Data) =
//      (
//        s.signal1.contact.asInstanceOf[c.type],
//        s.signal1.data.asInstanceOf[c.Data]
//      )
//
////    def unwrap[Contacts<:UniSet](s: S[Contacts]): (s.C, s.C#Data) = (s.contact, s.data)
////
////    def get2[Contacts<:UniSet](s: S[Contacts])(c: s.C): s.C#Data = unwrap(s)._2
////
////    def get[C<:Contact, Contacts<:UniSet](s: S[Contacts], c: C)(implicit cInContacts: C BelongsTo Contacts): Option[c.Data] = unwrap(s) match {
////      case (contact, data) if contact == c => Some(data.asInstanceOf[c.Data])
////      case _ => None
////    }
////
////    def getIterable[C<:Contact, Contacts<:UniSet](s: S[Contacts], c: C)(implicit cInContacts: C BelongsTo Contacts): Iterable[c.Data] = unwrap(s) match {
////      case (contact, data) if contact == c => Iterable.single(data.asInstanceOf[c.Data])
////      case _ => Iterable.empty
////    }
//////      if(scEqC == null) None else Some(sctEqCT(unwrap(s)._2))
////  //  def wrap[Cont<:Contact, Contacts](c: Cont, data: Cont#Data)(implicit cInContacts: Cont BelongsTo Contacts): S[Contacts] { type C = Cont }
////
////    def projection[Cont <: Contact, Contacts<:UniSet, B<:UniSet]
////    (s: S[Contacts]{ type C = Cont }, b: B)(implicit ev: Cont BelongsTo B)
////    : S[B] { type C = Cont }
//
////    def projection0[A <: UniSet, Signal <: S[A]]
////    (s: Signal, b: Predef.Set[Contact]): Iterable[S[Predef.Set[Contact]] { type C = s.C }] =
////      runtimeBelongsTo(s.contact, b).map{ sb => wrap[s.C, Predef.Set[Contact]](s.contact, s.data)(sb) }
//
////    def projection00Contact[A <: UniSet, B <: UniSet]
////    (b: B)(s: S[A]): Iterable[S[B] { type C = Contact }] = {
////      val contact = s.contact.asInstanceOf[Contact]
////      belongsTo0(contact, b).map{ sb => wrap[Contact, B](contact, s.data)(sb) }
////    }
//  }


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
//  abstract class unwrapSignalTrait[A <: UniSet, B <: UniSet, C <: UnionHelper[A,B]](val unionHelper: C) {
//   def apply(signalOnContacts: SignalOnContacts[unionHelper.Out])(implicit ops: SignalOnContactsOps[SignalOnContacts])
//    : (Iterable[SignalOnContacts[A]], Iterable[SignalOnContacts[B]])
//  }
//  class unwrapSignal2[A <: UniSet, B <: UniSet, C <: UnionHelper[A, B]](a: A, b: B)(implicit unionHelper: C) {
//      def apply(signalOnContacts: SignalOnContacts[unionHelper.Out])(implicit ops: SignalOnContactsOps[SignalOnContacts]): (Iterable[SignalOnContacts[A]], Iterable[SignalOnContacts[B]]) = {
//        (
//          ops.projection0[unionHelper.Out, A, SignalOnContacts[unionHelper.Out]](signalOnContacts, a),
//          ops.projection0[unionHelper.Out, B, SignalOnContacts[unionHelper.Out]](signalOnContacts, b)
//        )
//      }
//
//  }
//
//  class unwrapSignal2[A <: UniSet, B <: UniSet](a: A, b: B) {
//    def apply[C](implicit cinAB: C BelongsTo Union[A,B]): {
//      def apply(signalOnContacts: SignalOnContacts[Union[A,B]])(implicit ops: SignalOnContactsOps[SignalOnContacts])
//      : (Iterable[SignalOnContacts[A]], Iterable[SignalOnContacts[B]])
//    } = new {
//      def apply(signalOnContacts: SignalOnContacts[Union[A,B]])(implicit ops: SignalOnContactsOps[SignalOnContacts]): (Iterable[SignalOnContacts[A]], Iterable[SignalOnContacts[B]]) = {
//        (
//        ???, //  ops.projection0[unionHelper.Out, A, SignalOnContacts[unionHelper.Out]](signalOnContacts, a),
//        ??? //  ops.projection0[unionHelper.Out, B, SignalOnContacts[unionHelper.Out]](signalOnContacts, b)
//        )
//      }
//    }
//  }
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
  final class MySignalOnContact[Cont <: Contact](c: Cont)(d: Cont#Data) extends SignalOnContact {
    override type C = Cont
    val contact: C = c//Cont
    val data: C#Data = d
}


//  final class MySignal[Cont <: Contact, Contacts <: UniSet] private[MySignals] (val c: Cont, val d: Cont#Data, ev: Cont BelongsTo Contacts) extends MySignalOnContacts[Contacts] {
//    override type C = Cont
//
//    override def contact: C = c
//
//    override def contactIsInContacts: BelongsTo[Cont, Contacts] = ev
//
//    override def data: C#Data = d
//
//    override def equals(o: Any): Boolean = o match {
//      case other: MySignal[_,_] => c == other.c && d == other.d
//      case _ => false
//    }
//
//    override def hashCode(): Int = c.hashCode() + d.hashCode()
//
//    override def toString: String = s"MySignal($c, $d)"
//  }

//  implicit class MyContactOps[Cont <: Contact](val c: Cont) {
//    def wrap[Contacts <: UniSet]
//    (data: c.type#Data)
//    (implicit signalOnContactsOps: SignalOnContactsOps[SignalOnContacts], ev: c.type BelongsTo Contacts): SignalOnContacts[Contacts] { type C = c.type } =
//    ??? //  signalOnContactsOps.wrap[c.type, Contacts](c, data)
//    def wrapper[Contacts <: UniSet]
//    (implicit signalOnContactsOps: SignalOnContactsOps[SignalOnContacts], ev: c.type BelongsTo Contacts): (c.type#Data) => SignalOnContacts[Contacts] { type C = c.type } =
//      ??? // signalOnContactsOps.wrap[c.type, Contacts](c, _)
//  }
//  implicit class MyContactSetOps[Contacts <: UniSet](a: Contacts)(implicit signalOnContactsOps: SignalOnContactsOps[SignalOnContacts]) {
//    def wrap[Cont <: Contact]
//    (c: Cont)(data: c.type#Data)
//    (implicit ev: c.type BelongsTo Contacts): SignalOnContacts[Contacts] { type C = c.type } =
//      ??? // signalOnContactsOps.wrap[c.type, Contacts](c, data)
//  }
//  implicit object MySignalOps extends SignalOnContactsOps[SignalOnContacts] {
////    override def wrap[
////      Cont <: Contact,
////      Contacts <: UniSet
////    ](c: Cont, data: Cont#Data)
////     (implicit cInContacts: BelongsTo[Cont, Contacts])
////    : MySignalOnContacts[Contacts] {
////      type C = Cont
////    } = new MySignal[Cont, Contacts](c, data, cInContacts)
//
//    def projection[Cont <: Contact, Contacts<:UniSet, B<:UniSet]
//    (s: SignalOnContacts[Contacts]{ type C = Cont }, b: B)(implicit ev: Cont BelongsTo B): MySignalOnContacts[B]  { type C = Cont } = new MySignalOnContacts[B]{
//      override def contactIsInContacts: Cont BelongsTo B = ev
//
//      override type C = Cont
//
//      override def contact: C = s.contact
//
//      override def data: C#Data = s.data
//    }
//
//  }

  // Check if signals are compatible
//  implicit def eqContactsEqSignals[A<:UniSet, B<:UniSet](implicit aIsSubsetOfB: A ⊂ B): SignalContactsAreCompatibleOneWay[SignalOnContacts[A], SignalOnContacts[B]] =
//    (s: SignalOnContacts[A]) => ??? // new MySignal[s.C, B](s.contact, s.data, inferEBelongsToBIfEBelongsToASubset[s.C, A, B](s.contactIsInContacts, aIsSubsetOfB))

  // wraps a function into a component with a single input and single output.
  def lift[In <: Contact, Out <: Contact]
    (in: In, out: Out)
    (f: In#Data => Out#Data)
    : Singleton[In] >> Singleton[Out] =
  {
    signalOnContactIn =>
      val d = signalOnContactIn.safeUnwrap2[In]
      val res = f(d)
      val s1: MySignalOnContact[Out] = new MySignalOnContact(out)(res)
      Iterable.single(signal[Singleton[Out]](s1))
  }

  def liftIterable[In <: Contact, Out <: Contact]
    (in: In, out: Out)
    (f: In#Data => Iterable[Out#Data])
    : Singleton[In] >> Singleton[Out] =
  {
    signalOnContactIn =>
      val d = signalOnContactIn.safeUnwrap2[In]
      val res = f(d)
      res.map(d => signal[Singleton[Out]](new MySignalOnContact(out)(d)))
  }

  // wraps a single contact to be both input and output.
  def trivialLift[C <: Contact]
    (c: C)
    : Si[C] >> Si[C] =
    Iterable.single

  // identity function to simply pass signal
  def identity[In <: Contact, Out <: Contact]
    (in: In, out: Out)
    (implicit
      ev: In#Data <:< Out#Data
    )
    : Si[In] >> Si[Out]
    = lift(in,out)(ev)
}
