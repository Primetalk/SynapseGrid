package ru.primetalk.contacts.core

trait Signals extends TypeSets {

  trait Contact {
    type T
  }


  trait Signal {
    type Contact
  }

  type SignalProcessor = Signal => Iterable[Signal]

  trait SignalOnContacts0 {
    type C <: Contact
    def contact: C
    def data: C#T
  }
  // This signal has phantom type argument that means that it is compatible with
  // the given set of contacts. In particular, C should be in Contacts
  trait SignalOnContacts[Contacts <: TypeSet] extends SignalOnContacts0 {
    def contactIsInContacts: C BelongsTo Contacts
  }

  // typeclass for dealing with signals
  trait SignalOnContactsOps[S[_<:TypeSet]<:SignalOnContacts0] {
    def unwrap[Contacts<:TypeSet](s: S[Contacts]): (s.C, s.C#T) = (s.contact, s.data)
    def get[C<:Contact, Contacts<:TypeSet](s: S[Contacts], c: C): Option[C#T] = unwrap(s) match {
      case (contact, data) if contact == c => Some(data.asInstanceOf[C#T])
      case _ => None
    }
//      if(scEqC == null) None else Some(sctEqCT(unwrap(s)._2))
    def wrap[Cont<:Contact, Contacts<:TypeSet](c: Cont, data: Cont#T)(implicit cInContacts: Cont BelongsTo Contacts): S[Contacts] { type C = Cont }
  }



}

trait MySignals extends Signals {
  sealed trait MySignalOnContacts[Contacts <: TypeSet] extends SignalOnContacts[Contacts]

  final class MySignal[Cont <: Contact, Contacts <: TypeSet] private[MySignals] (val c: Cont, val d: Cont#T, ev: Cont BelongsTo Contacts) extends MySignalOnContacts[Contacts] {
    override type C = Cont

    override def contact: C = c

    override def contactIsInContacts: BelongsTo[Cont, Contacts] = ev

    override def data: C#T = d

    override def equals(o: Any): Boolean = o match {
      case other: MySignal[_,_] => c == other.c && d == other.d
      case _ => false
    }

    override def hashCode(): Int = c.hashCode() + d.hashCode()

    override def toString: String = s"MySignal($c, $d)"
  }

  implicit object MySignalOps extends SignalOnContactsOps[MySignalOnContacts] {
    override def wrap[Cont <: Contact, Contacts <: TypeSet](c: Cont, data: Cont#T)(implicit cInContacts: BelongsTo[Cont, Contacts]): MySignalOnContacts[Contacts] {
      type C = Cont
    } = new MySignal[Cont, Contacts](c, data, cInContacts)
  }

  def lift[In <: Contact, Out <: Contact](in: In, out: Out)(f: In#T => Out#T)(
    implicit signalOnContactsOps: SignalOnContactsOps[MySignalOnContacts]
  ):
  MySignalOnContacts[In +: ∅] => Iterable[MySignalOnContacts[Out +: ∅]] = signalOnContactIn =>
  {
    signalOnContactsOps.get[In, In +: ∅](signalOnContactIn, in) match {
      case Some(dataIn) =>
        val res = f(dataIn)
        Iterable.single(signalOnContactsOps.wrap(out, res))
    }
  }

}