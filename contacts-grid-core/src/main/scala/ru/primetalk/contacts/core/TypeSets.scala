package ru.primetalk.contacts.core

import scala.annotation.implicitNotFound

trait A {
  type Component[InputShape, OutputShape]
  // apocalisp
  sealed trait Bool {
    type If[TrueBranch, FalseBranch]
  }
  sealed trait True extends Bool {
    type If[TrueBranch, FalseBranch] = TrueBranch
  }
  sealed trait False extends Bool {
    type If[TrueBranch, FalseBranch] = FalseBranch
  }
  trait MyProg0 {
    type MyVarType[Param <: Bool] = Param#If[Int, String]
  }
  implicitly[MyProg0#MyVarType[True] =:= Int]
  implicitly[MyProg0#MyVarType[False] =:= String]
}
trait B {
  val a: Int
  val b: Int
 // object contactA extends Contact[Int]
}
sealed trait TypeSets2 {
  sealed trait TypeSet
  case object Empty extends TypeSet
  // deduplicate!
  final case class ConsTypeSet[E, S <: TypeSet](e: E,s: S) extends TypeSet
  // ∅ - \u2205
  type ∅ = Empty.type
  type Empty = Empty.type
  // This class enumerates elements of the set.
  // In order to avoid duplicates, we make constructor private and use
  // the trick with the implicit priorities.
  // In this trait we define general case, and in
  // the next trait we define the case when element belongs to the set.
  //case class ConsSet[E, S <: TypeSet](element: E, set: S) extends TypeSet
  // ⊕ - \u2295
//  type +:[E, S<:TypeSet] = AddElementHelper[E, S]#Out
  sealed trait AddWrapper[E, S<:TypeSet] {
    type AuxPlus <: TypeSet
    def auxPlus(e: E,s: S): AuxPlus
  }
  //type +:[E, S<:TypeSet] = AddWrapper#AuxPlus[E, S]

//
//  sealed trait AddWrapperHead extends AddWrapper {
//    type AuxPlus[E, S<:TypeSet] = E ConsTypeSet S
//  }
//
//  sealed trait AddWrapperIgnore extends AddWrapper {
//    type AuxPlus[E, S<:TypeSet] = S
//  }

  def addElement[E, S<:TypeSet](e: E, s: S)(implicit ev: AddWrapper[E,S]#AuxPlus): ev.type =
    ev//addWrapper.auxPlus(e,s)

  implicit def getEv[E, S<:TypeSet](e: E, s: S)(implicit addWrapper: AddWrapper[E,S]): addWrapper.AuxPlus =
    addWrapper.auxPlus(e,s)

  implicit def getAddWrapper[E, S<:TypeSet] = new AddWrapper[E,S] {
    override type AuxPlus = E ConsTypeSet S
    def auxPlus(e: E,s: S): AuxPlus = ConsTypeSet[E,S](e,s)

  }
  //  type +:[E, S<:TypeSet] <: TypeSet
  //sealed trait +:[E, S<:TypeSet]// = AddElementHelper[E, S]#Out

  @implicitNotFound("Couldn't add element to set")
  sealed trait AddElementHelper[E, S<:TypeSet] {
    type Out <: TypeSet
    //def out(e: E, s: S): Out
    //def unapply(es: Out): (E, S)
  }

  object AddElementHelper {
    implicit def AddElementHelperAddElementToTypeSet[E, S<:TypeSet] =
      new AddElementHelper[E,S] {
        type Out = E ConsTypeSet S
        // def out(e: E, s: S): Out = ConsSet(e, s)
        // def unapply(es: Out): (E, S) = (es.element, es.set)
      }
  }

  @implicitNotFound("Couldn't prove that element belongs to set")
  sealed trait BelongsTo[Element, S <: TypeSet]
  // ∊ - \u220A
  type ∊[Element, S <: TypeSet] = BelongsTo[Element, S]
  object BelongsTo {
    implicit def elementIsHeadOfTypeSet0[E, S <: TypeSet]: E ∊ (E ConsTypeSet S) =
      new BelongsTo[E, E ConsTypeSet S] {}
//    implicit def elementIsHeadOfTypeSet[E, S <: TypeSet]: E ∊ (E +: S) =
//      new BelongsTo[E, E +: S] {}
    implicit def elementBelongsToTailOfTypeSet0[E, H, S <: TypeSet](implicit b: E ∊ S): E ∊ (H ConsTypeSet S) =
      new BelongsTo[E, H ConsTypeSet S] {}
//    implicit def elementBelongsToTailOfTypeSet[E, H, S <: TypeSet](implicit b: E ∊ S): E ∊ (H +: S) =
//      new BelongsTo[E, H +: S] {}
  }
//  trait ExtractorHelper[E, S<: TypeSet] {
//    def extract(es: E + S):
//  }
//  def head[E, S<: TypeSet](implicit ev: AddElementHelper[E,S]): ExtractorHelper = ev.unapply(es)._1
  def typeSet[S<:TypeSet](implicit s: S): S = s
}
sealed trait TypeSets1 extends TypeSets2 {
//  sealed trait TypeSetRepr[S<:TypeSet]
////  type TypeSetRepr[S<:TypeSet] = List[Any]
//  case object TSNil extends TypeSetRepr[Empty]
//  case class TSCons[E, S<:TypeSet](e: E, typeSetRepr: TypeSetRepr[S]) extends TypeSetRepr[E+:S]
//  object TypeSetRepr {
//    implicit def emptyRepr: TypeSetRepr[Empty] = TSNil
//
//    implicit def consRepr[E, S <: TypeSet](implicit e: E, s: TypeSetRepr[S]): TypeSetRepr[E+:S] = TSCons[E, S](e, s)
//  }
//  def toList[S<:TypeSet](implicit lst: TypeSetRepr[S]): TypeSetRepr[S] = lst
}
sealed trait TypeSets0 extends TypeSets1 {
  implicit def getAddWrapperIgnore[E, S<:TypeSet](implicit ev: E ∊ S ) = new AddWrapper[E,S] {
    override type AuxPlus = S
    def auxPlus(e: E, s: S): AuxPlus = s
  }

  implicit def AddElementHelperAddElementToTypeSet[E, S<:TypeSet] =
    new AddElementHelper[E,S] {
      type Out = E ConsTypeSet S
     // def out(e: E, s: S): Out = ConsSet(e, s)
      // def unapply(es: Out): (E, S) = (es.element, es.set)
    }

//  implicit def addElement[E, S<:TypeSet](implicit e: E, s: S, ev: AddElementHelper[E, S]): E +: S =
//    ev.out(e,s)
}
trait TypeSets extends TypeSets0 {

  implicit def AddElementHelperAddElementToTypeSetWhenEBelongsToS[E, S<:TypeSet](implicit ev: E ∊ S) =
    new AddElementHelper[E,S] {
      type Out = S
//      def out(e: E, s: S): Out = s
      //def unapply(es: Out): (E, S) = (es.element, es.set)

    }

  // ∪ \u222A
  type ∪[A <: TypeSet, B <: TypeSet] = UnionHelper[A, B]#Out

  sealed trait UnionHelper[A <: TypeSet, B <: TypeSet] {
    type Out <: TypeSet
    def out(a: A, b: B): Out
  }

  object UnionHelper {
    implicit def caseAIsEmpty[B <: TypeSet]: UnionHelper[∅, B] =
      new UnionHelper[∅, B] {
        type Out = B
        def out(a: ∅, b: B): Out = b
      }
//    implicit def caseAHeadTail[E, S<: TypeSet, B <: TypeSet](implicit ev: UnionHelper[S,B], addEl: E + B): UnionHelper[E + S, B] =
//      new UnionHelper[E + S, B] {
//        type Out = E + ev.Out
//        def out(a: E + S, b: B): Out = addEl.out(a.) + b
//      }
  }
}
