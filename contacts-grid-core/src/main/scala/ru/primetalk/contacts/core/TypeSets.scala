package ru.primetalk.contacts.core

import scala.annotation.implicitNotFound

sealed trait TypeSets1 {
  //
  sealed trait TypeSet extends Serializable with Product
  case object Empty extends TypeSet
  type Empty = Empty.type
  // ∅ - \u2205 - synonyms
  type ∅ = Empty
  val ∅ : Empty = Empty
  // This class enumerates elements of the set.
  // In order to avoid duplicates, we make constructor private and
  // take special measures for deduplication.
  // Deduplication trick:
  // The trick with the Scala implicit priorities.
  // In this trait TypeSets2 we define general case, and in
  // the next trait TypeSets1 we define the case when element belongs to the set.
  final case class ConsTypeSet[E, S <: TypeSet] private (e: E,s: S) extends TypeSet
  // This is an operator for representing sets without duplications
  type +:[E, S <: TypeSet] = ConsTypeSet[E, S]
  // ⊕ - \u2295
  sealed trait AddElement[E, S<:TypeSet] {
    type AddElement <: TypeSet
    def apply(e: E, s: S): AddElement
  }

  def addElement[E, S<:TypeSet](e: E, s: S)(implicit addWrapper: AddElement[E,S]): addWrapper.AddElement =
    addWrapper.apply(e,s)

  trait TypeSetOps2[S<:TypeSet] {
    def s: S
    def +:[E](e: E)(implicit addWrapper: AddElement[E,S]): addWrapper.AddElement =
      addWrapper.apply(e,s)
  }
  implicit def getAddElementPriority2[E, S<:TypeSet]: AddElement[E, S] {
    type AddElement = ConsTypeSet[E, S]
  } = new AddElement[E,S] {
    override type AddElement = E ConsTypeSet S
    override def apply(e: E, s: S): AddElement = ConsTypeSet[E,S](e,s)
  }
//
//  @implicitNotFound("Couldn't add element to set")
//  sealed trait AddElementHelper[E, S<:TypeSet] {
//    type Out <: TypeSet
//    //def out(e: E, s: S): Out
//    //def unapply(es: Out): (E, S)
//  }
//
//  object AddElementHelper {
//    implicit def AddElementHelperAddElementToTypeSet[E, S<:TypeSet]: AddElementHelper[E, S] {
//      type Out = E ConsTypeSet S
//    } =
//      new AddElementHelper[E,S] {
//        type Out = E ConsTypeSet S
//        // def out(e: E, s: S): Out = ConsSet(e, s)
//        // def unapply(es: Out): (E, S) = (es.element, es.set)
//      }
//  }


  @implicitNotFound("Couldn't prove that each element of TypeSet is subtype the given Up type")
  sealed trait EachElementIsSubtype[Up, TypeSet]
  object EachElementIsSubtype {
    implicit def Empty[Up]: EachElementIsSubtype[Up, Empty] =
      new EachElementIsSubtype[Up, Empty] {}
    implicit def Cons[Up, E <: Up, S <: TypeSet](implicit ev: EachElementIsSubtype[Up, S]): EachElementIsSubtype[Up, ConsTypeSet[E, S]] =
      new EachElementIsSubtype[Up, ConsTypeSet[E, S]] {}
  }

  @implicitNotFound("Couldn't prove that element belongs to set")
  sealed trait BelongsTo[Element, S <: TypeSet]
  // ∊ - \u220A
  type ∊[Element, S <: TypeSet] = BelongsTo[Element, S]
  object BelongsTo {
    implicit def elementIsHeadOfTypeSet0[E, S <: TypeSet]: E ∊ (E ConsTypeSet S) =
      new BelongsTo[E, E ConsTypeSet S] {}
    implicit def elementBelongsToTailOfTypeSet0[E, H, S <: TypeSet](implicit b: E ∊ S): E ∊ (H ConsTypeSet S) =
      new BelongsTo[E, H ConsTypeSet S] {}
  }
  @implicitNotFound("Couldn't prove that predicate holds true for each element")
  sealed trait ForAll[P[_], S<: TypeSet]
  object ForAll {
    implicit def empty[P[_]]: ForAll[P, Empty] = new ForAll[P, Empty] {}
    implicit def cons[P[_], E, S<: TypeSet](implicit p: P[E], forAll: ForAll[P, S]): ForAll[P, E +: S] = new ForAll[P, E +: S] {}
  }
  @implicitNotFound("Couldn't prove that predicate holds true for each element")
  sealed trait Exists[P[_], S<: TypeSet]
  object Exists {
    implicit def consHead[P[_], E, S<: TypeSet](implicit p: P[E]): Exists[P, E +: S] = new Exists[P, E +: S] {}
    implicit def consTail[P[_], E, S<: TypeSet](implicit exists: Exists[P, S]): Exists[P, E +: S] = new Exists[P, E +: S] {}
  }

  @implicitNotFound("Couldn't prove that typesets are equal")
  trait TypeSetEq[A<:TypeSet, B<:TypeSet]
  object TypeSetEq {
    implicit def typeSetEq[A<:TypeSet, B<:TypeSet](implicit ev1: IsSubset[A,B], ev2: IsSubset[B,A]): TypeSetEq[A, B] = new TypeSetEq[A, B]{}
  }
  /**
    * IsSubset type-level operation.
    * O(N^^2)
    * TODO: implement O(N) hash-based implementation.
    */
  @implicitNotFound("Couldn't prove that set is in another set")
  sealed trait IsSubset[Subset <: TypeSet, SuperSet <: TypeSet]
  // ⊂ - \u2282
  type ⊂[Subset <: TypeSet, SuperSet <: TypeSet] = IsSubset[Subset, SuperSet]

  object IsSubset {
    implicit def empty[SuperSet2<:TypeSet]:
      IsSubset[∅, SuperSet2] = new IsSubset[∅, SuperSet2] {}
    implicit def cons[E, S <: TypeSet, SuperSet<:TypeSet](implicit headBelongs: E ∊ SuperSet, tailIsSubset: S ⊂ SuperSet):
      IsSubset[E ConsTypeSet S, SuperSet] = new IsSubset[E ConsTypeSet S, SuperSet]{}
  }
}
sealed trait TypeSets0 extends TypeSets1 {
  implicit def getAddElementPriority1[E, S<:TypeSet](implicit ev: E ∊ S ): AddElement[E, S] {
    type AddElement = S
  } = new AddElement[E,S] {
    override type AddElement = S
    def apply(e: E, s: S): AddElement = s
  }
}
trait TypeSets extends TypeSets0 {

  // ∪ \u222A
  def ∪[A <: TypeSet, B <: TypeSet](a: A, b: B)(implicit unionHelper: UnionHelper[A,B]): unionHelper.Out = unionHelper.out(a,b)

  type ∪[A <: TypeSet, B <: TypeSet] = UnionHelper[A, B]#Out

  sealed trait UnionHelper[A <: TypeSet, B <: TypeSet] {
    type Out <: TypeSet
    def out(a: A, b: B): Out
  }

  object UnionHelper {
    implicit def empty[B <: TypeSet]: UnionHelper[∅, B] =
      new UnionHelper[∅, B] {
        type Out = B
        def out(a: ∅, b: B): Out = b
      }
    implicit def cons[E, S<: TypeSet, B <: TypeSet](implicit ev: S UnionHelper B, addEl: E AddElement (S ∪ B)): UnionHelper[E +: S, B] =
      new UnionHelper[E +: S, B] {
        type Out = addEl.AddElement
        def out(a: E +: S, b: B): Out = addEl.apply(a.e, ev.out(a.s,b))
      }
  }


  implicit class TypeSetOps[S<:TypeSet](val s: S) extends TypeSetOps2[S] {
    def ∪[B <: TypeSet](b: B)(implicit unionHelper: UnionHelper[S,B]): unionHelper.Out = unionHelper.out(s, b)
  }
}
