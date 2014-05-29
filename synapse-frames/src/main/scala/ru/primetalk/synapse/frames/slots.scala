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
 * Created: 11.05.14, zhizhelev
 */
package ru.primetalk.synapse.frames

import types._

object slots {

  trait SlotId0 {
    type Type
  }

  trait SlotId[T] extends SlotId0 {
    type Type = T

    def :=(value: Type) = SlotValue(this, value)
  }

  type SlotIdExistential = SlotId[_]

  case class SlotValue[T, S <: SlotId[T]](slotId: S, value: T)

  type SlotValueExistential = SlotValue[T, S] forSome {type T; type S <: SlotId[T]}
  /** Sequence of properties. */
  sealed trait SlotSeq {
    self =>
    /** This type is used to accumulate slot types.
      * To check that some slot belongs to this slot set it
      * is enough to require implicit argument:
      * (implicit ev: SlotUnion <:< P ) */
    type SlotUnion

    /** The type of value of the slot sequence.
      * */
    type Record <: types.HList

    def slots: List[SlotId0]

    def ::[P <: SlotId0](slotId: P) = new ::[P, self.type](slotId, self)
  }

  case object SNil extends SlotSeq {
    type SlotUnion = Any
    type Record = HNil

    def slots: List[SlotId0] = List()
  }

  type SNil = SNil.type

  case class ::[P <: SlotId0, PS <: SlotSeq](slotId: P, tail: PS) extends SlotSeq {
    type SlotUnion = P with PS#SlotUnion

    type Record = types.::[P#Type, PS#Record]

    def slots: List[SlotId0] = slotId :: tail.slots
  }

  /** An instance builder can contain SlotValues for any subset of properties defined in a SlotSeq. */
  sealed trait SlotSeqValueBuilder {
    self =>
    /** The slot sequence of the result */
    type SlotSeq1 <: SlotSeq

    def slotSeq: SlotSeq1

    /** values of the result. */
    def values: SlotSeq1#Record

    /** pairs for runtime processing */
    def list: List[SlotValueExistential]

    private
    lazy val map = list.map(sv => (sv.slotId: SlotId0, sv.value)).toMap

    def get[SlotType <: SlotId[_]](slotId: SlotType)(implicit ev: SlotSeq1#SlotUnion <:< SlotType) =
      map(slotId).asInstanceOf[SlotType#Type]

    def ::[T, S1 <: SlotId[T]](slotValue: SlotValue[T, S1]) =
      PairSlotSeqValueBuilder[T, S1, self.type](slotValue, self)


  }

  //
  /** An empty instance for the given property set.
    * We can then expand the instance by adding more property values. */
  case object EmptySlotSeqValueBuilder extends SlotSeqValueBuilder {
    type SlotSeq1 = SNil

    def slotSeq: SlotSeq1 = SNil

    def values: SlotSeq1#Record = HNil

    def list: List[SlotValueExistential] = Nil
  }

  case class PairSlotSeqValueBuilder[T, S1 <: SlotId[T], I <: SlotSeqValueBuilder](head: SlotValue[T, S1], tail: I) extends SlotSeqValueBuilder {
    type SlotSeq1 = S1 :: I#SlotSeq1

    def slotSeq: SlotSeq1 = (head.slotId :: tail.slotSeq).asInstanceOf[SlotSeq1]

    def values: SlotSeq1#Record = (head.value :: tail.values).asInstanceOf[SlotSeq1#Record]

    def list: List[SlotValueExistential] = head :: tail.list
  }


  case class SlotDelta[T: Numeric, S <: SlotId[T]](slotId: S, delta: T) {
    def addTo(oldValue: SlotValue[T, S]) =
      SlotValue(slotId, implicitly[Numeric[T]].plus(oldValue.value, delta))
  }

  implicit class SlotValueEx[T: Numeric, S <: SlotId[T]](slotValue: SlotValue[T, S]) {
    def +(delta: T) =
      SlotValue(slotValue.slotId, implicitly[Numeric[T]].plus(slotValue.value, delta))
  }

}
