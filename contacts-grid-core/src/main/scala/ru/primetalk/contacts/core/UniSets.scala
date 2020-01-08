package ru.primetalk.contacts.core

import scala.annotation.implicitNotFound

// Here we define operations on sets irrespective of any evidences
sealed trait UniSetsBase {

  sealed trait UniSet

  sealed trait Empty extends UniSet
  type ∅ = Empty // \u2205 - synonyms
  sealed trait Singleton[E] extends UniSet
  sealed trait Universum extends UniSet

  sealed trait Union[A <: UniSet, B <: UniSet] extends UniSet
  type ∪[A <: UniSet, B <: UniSet] = Union[A, B] // \u222A

  sealed trait Intersection[A <: UniSet, B <: UniSet] extends UniSet
  type ∩[A <: UniSet, B <: UniSet] = Intersection[A, B]

  /**
    * A possible variant:
    * {{{
    *   A \ B = A ∩ Not[B]
    * }}}
    */
  sealed trait Subtract[A <: UniSet,B <: UniSet] extends UniSet
  type Not[A <: UniSet] = Subtract[Universum, A] // Universum \ A

  /** TODO:
    * {{{
    *   A ^ B = A ∪ B \ A ∩ B
    * }}}
    */
  sealed trait Xor[A <: UniSet,B <: UniSet] extends UniSet

  /** A possible variant:
    * {{{
    *   e +: S = {e} ∪ S
    * }}}
    * Looks like we don't often use `Insert`, so we may replace it according to the above equation.
    */
  sealed trait Insert[E, S <: UniSet] extends UniSet
//  type Insert[E, S <: UniSet] = Union[Singleton[E], S]

  /** Applies `F` to each element of the set. */
  sealed trait UniMap[S <: UniSet, F[_]] extends UniSet
}
sealed trait UniProperties extends UniSetsBase {
  // Public relation that can be inferred from operations on sets.
  @implicitNotFound("Couldn't prove that element belongs to set")
  sealed trait BelongsTo[Element, Set] {
//    def get(s: Set): Element
  }

  type ∊[Element, Set] = BelongsTo[Element, Set] // \u220A

  @implicitNotFound("Couldn't prove that set is subset of another one set")
  sealed trait IsSubSetOf[A <: UniSet,B <: UniSet]
  type ⊂[A <: UniSet,B <: UniSet] = IsSubSetOf[A,B]
  type <=[A <: UniSet,B <: UniSet] = IsSubSetOf[A,B]
  // Implementation of BelongsTo using subsets
  type BelongsToViaSubset[E,S <: UniSet] = IsSubSetOf[Singleton[E], S]
  // Collects runtime elements of the set
  sealed trait Render[Up, A <: UniSet] {
    def elements: Set[Up]
  }

  @implicitNotFound("Couldn't prove that each element is subtype of the given one")
  sealed trait EachElementIsSubtype[Up, S <: UniSet]

  @implicitNotFound("Couldn't prove that predicate holds true for each element")
  sealed trait ForAll[P[_], S<: UniSet]

  @implicitNotFound("Couldn't prove that predicate holds true for at least one element")
  sealed trait Exists[P[_], S<: UniSet]

  @implicitNotFound("Couldn't prove that sets are equal")
  sealed trait Equal[A<:UniSet, B<:UniSet]
//  {
//    def aToB(a: A): B
//    def bToA(b: B): A
//  }
  @implicitNotFound("Couldn't prove that the set has only one element")
  sealed trait Cardinality1[Up, S <: UniSet] {
    // the type of the single element of the set
    type Element <: Up
  }
}
sealed trait BelongsToLowPriority extends UniProperties {
  // Here we define sets in a "non-constructive way". In particular, we
  // use "belongs to" relation as a primary definition of a set.
  // All other operations should define BelongsTo relation.


  // there is no instance of BelongsTo for Empty
  implicit def SingletonBelongsTo[Element]: BelongsTo[Element, Singleton[Element]] = new BelongsTo[Element, Singleton[Element]] {}

  implicit def UnionBelongsToB[Element, A <: UniSet,B <: UniSet](implicit eb: BelongsTo[Element, B]): BelongsTo[Element, Union[A,B]] = new BelongsTo[Element, Union[A,B]] {}
  // see also high priority

  implicit def IntersectionBelongsTo[Element, A <: UniSet,B <: UniSet](implicit ea: BelongsTo[Element, A], eb: BelongsTo[Element, B]): BelongsTo[Element, Intersection[A,B]] = new BelongsTo[Element, Intersection[A,B]] {}

  implicit def universum[E]: BelongsTo[E, Universum] = new BelongsTo[E, Universum] {}


  implicit def SubtractBelongsToA[E, A <: UniSet, B <: UniSet](implicit ea: BelongsTo[E, A]): BelongsTo[E,Subtract[A,B]] = new BelongsTo[E,Subtract[A,B]] {}
  implicit def SubtractBelongsToAB[E, A <: UniSet, B <: UniSet](implicit ea: BelongsTo[E, A], eb: BelongsTo[E,B]): BelongsTo[E,Subtract[A,B]] = ???

  implicit def XorBelongsToA[E, A <: UniSet, B <: UniSet](implicit ea: BelongsTo[E, A]): BelongsTo[E,Xor[A,B]] = new BelongsTo[E,Xor[A,B]] {}
  implicit def XorBelongsToB[E, A <: UniSet, B <: UniSet](implicit eb: BelongsTo[E, B]): BelongsTo[E,Xor[A,B]] = new BelongsTo[E,Xor[A,B]] {}

  // checking S has lower priority.
  // See also InsertEBelongsTo
  implicit def InsertSBelongsTo[Element, S <: UniSet](implicit s: BelongsTo[Element, S]): BelongsTo[Element, Insert[Element, S]] = new BelongsTo[Element, Insert[Element, S]] {}

  // produce runtime evidence when needed
  def runtimeBelongsTo[A, B<:A](b: B, s: Set[A]): Option[BelongsTo[B, Set[A]]] =
    if(s.contains(b)) Some(new BelongsTo[B, Set[A]] {})
    else None

  /** {{{ e ∊ a => f(e) ∊ F(a) }}} */
  implicit def MapBelongsToB[E, A <: UniSet, F[_]](implicit ea: BelongsTo[E, A]): BelongsTo[F[E],UniMap[A,F]] = new BelongsTo[F[E],UniMap[A,F]] {}

//  implicit def runtimeSingleton[E<: S]
}
// fallback to by-element check of subsets.
sealed trait ElementwiseIsSubSetOf extends UniProperties {
  implicit def singletonIsSubset[E, S <: UniSet](implicit es: BelongsTo[E, S]): IsSubSetOf[Singleton[E], S] = new IsSubSetOf[Singleton[E], S] {}
  implicit def insertIsSubset[E, A <: UniSet, S <: UniSet](implicit es: BelongsTo[E, S], as: IsSubSetOf[A, S]): IsSubSetOf[Insert[E, A], S] = new IsSubSetOf[Insert[E, A], S] {}
}


sealed trait IsSubSetOfLowPriority extends ElementwiseIsSubSetOf  {

  // See also reflectivity below for Empty <= Empty
  implicit def EmptyIsSubSetOf[A<:UniSet]: IsSubSetOf[Empty, A] = new IsSubSetOf[Empty, A] {}
  //
  implicit def UnionABIsSubsetOfS[A <: UniSet, B <: UniSet, S <: UniSet](implicit as: IsSubSetOf[A, S], bs: IsSubSetOf[B, S]): IsSubSetOf[Union[A,B], S] = new IsSubSetOf[Union[A,B], S] {}

  implicit def subtractFromSmallerIsEmpty[A <: UniSet, B <: UniSet](implicit ab: IsSubSetOf[A,B]): IsSubSetOf[Subtract[A,B], Empty] = new IsSubSetOf[Subtract[A,B], Empty] {}

  implicit def intersectionOfSingletonsIsEmpty[E1, E2]: IsSubSetOf[Intersection[Singleton[E1], Singleton[E2]], Empty] = new IsSubSetOf[Intersection[Singleton[E1], Singleton[E2]], Empty] {}
  implicit def subtractFromNonIntersectingIsTheSame[A <: UniSet, B <: UniSet](implicit ab: IsSubSetOf[Intersection[A,B], Empty]): IsSubSetOf[Subtract[A,B], A] = new IsSubSetOf[Subtract[A,B], A] {}
  implicit def SIsSubsetOfUnionAB_A[S <: UniSet, A <: UniSet, B <: UniSet](implicit smabb: IsSubSetOf[Subtract[S, A], B]): IsSubSetOf[S, Union[A,B]] = new IsSubSetOf[S, Union[A,B]] {}

  //TODO  implicit def SIsSubsetOfUnionAB[S <: UniSet, A <: UniSet, B <: UniSet](implicit as: IsSubSetOf[A, S], bs: IsSubSetOf[B, S]): IsSubSetOf[Union[A,B], S] = new BelongsTo[Element, Union[A,B]] {}

  //  sealed trait Union[A <: UniSet, B <: UniSet]
//  sealed trait Intersection[A <: UniSet,B <: UniSet] <: UniSet

  // A \ B ⊂ C <=  A ⊂ B + C
//  implicit def aMinusBIsInC[A <: UniSet, B <: UniSet, C <: UniSet](implicit ev: IsSubSetOf[A, Union[B,C]]): IsSubSetOf[Subtract[A,B], C] = new IsSubSetOf[Subtract[A,B], C] {}
  implicit def SIsSubSetOfIntersectionAB[S <: UniSet, A <: UniSet, B <: UniSet](implicit sa: IsSubSetOf[S,A], sb: IsSubSetOf[S,B]): IsSubSetOf[S, Intersection[A,B]] = new IsSubSetOf[S, Intersection[A,B]] {}
  //
  implicit def SubIsSubsetOfA[A<:UniSet, B<:UniSet]: IsSubSetOf[Subtract[A,B], A] = new IsSubSetOf[Subtract[A,B], A] {}
  implicit def SubAAIsSubsetOfEmpty[A<:UniSet]: IsSubSetOf[Subtract[A,A], Empty] = new IsSubSetOf[Subtract[A,A], Empty] {}

  implicit def AIsSubSetOfXor[A<:UniSet, B<:UniSet]: IsSubSetOf[Subtract[A,B], Xor[A,B]] = new IsSubSetOf[Subtract[A,B], Xor[A,B]] {}
  implicit def BIsSubSetOfXor[A<:UniSet, B<:UniSet]: IsSubSetOf[Subtract[B,A], Xor[A,B]] = new IsSubSetOf[Subtract[B,A], Xor[A,B]] {}

  implicit def EIsSubSetOfInsert[E, S <: UniSet]: IsSubSetOf[Singleton[E], Insert[E,S]] = new IsSubSetOf[Singleton[E], Insert[E,S]] {}
  implicit def SIsSubSetOfInsert[E, S <: UniSet]: IsSubSetOf[S, Insert[E,S]] = new IsSubSetOf[S, Insert[E,S]] {}
  implicit def EInsIsSubSetOfE[E, S <: UniSet]: IsSubSetOf[Insert[E,Empty], Singleton[E]] = new IsSubSetOf[Insert[E,Empty], Singleton[E]] {}
  implicit def MapIsSubSetOfE[A<:UniSet, B<:UniSet, F[_]](implicit ab: IsSubSetOf[A, B]): IsSubSetOf[UniMap[A,F], UniMap[B,F]] = new IsSubSetOf[UniMap[A,F], UniMap[B,F]] {}
}

sealed trait IsSubSetOfHighPriority extends IsSubSetOfLowPriority {
  implicit def ReflectivityIsSubSetOf[A <: UniSet]: IsSubSetOf[A, A] = new IsSubSetOf[A, A] {}
  implicit def TransitivityIsSubSetOf[A <: UniSet, B <: UniSet, C <: UniSet](implicit ab: IsSubSetOf[A,B], bc: IsSubSetOf[B,C]): IsSubSetOf[A, C] = new IsSubSetOf[A, C] {}
  //  implicit def SingletonIsSubSetOf[Element]: IsSubSetOf[Singleton[Element], Singleton[Element]] = new IsSubSetOf[Singleton[Element], Singleton[Element]] {}

  implicit def UniversumIsSuperSetOf[A<:UniSet]: IsSubSetOf[A, Universum] = new IsSubSetOf[A, Universum] {}
  implicit def SIsSubsetOfUnionAB_B[S <: UniSet, A <: UniSet, B <: UniSet](implicit smbba: IsSubSetOf[Subtract[S, B], A]): IsSubSetOf[S, Union[A,B]] = new IsSubSetOf[S, Union[A,B]] {}
  implicit def SubIsSubsetOfNotB[A<:UniSet, B<:UniSet]: IsSubSetOf[Subtract[A,B], Not[B]] = new IsSubSetOf[Subtract[A,B], Not[B]] {}

  // Universal properties of Intersection
  implicit def intersectionOfTheSameSetIsTheSameSet[S <: UniSet]: IsSubSetOf[Intersection[S, S], S] = new IsSubSetOf[Intersection[S, S], S] {}
  // Universal properties of Union
  implicit def UnionOfTheSameSetIsTheSameSet[S <: UniSet]: IsSubSetOf[Union[S, S], S] = new IsSubSetOf[Union[S, S], S] {}

  // Universal properties of Empty
  implicit def subtractEmptyIsTheSame[A <: UniSet]: IsSubSetOf[Subtract[A,Empty], A] = new IsSubSetOf[Subtract[A,Empty], A] {}
  implicit def intersectEmptyIsEmpty[A <: UniSet]: IsSubSetOf[Intersection[A,Empty], Empty] = new IsSubSetOf[Intersection[A,Empty], Empty] {}

}

sealed trait RenderLowPriority extends UniProperties {
  protected final case class RenderImpl[Up, S <: UniSet](elements: Set[Up]) extends Render[Up, S]
  implicit def EmptyRender[Up]: Render[Up, Empty] = RenderImpl[Up, Empty](Set())
  implicit def SingletonRender[Up, E<:Up:ValueOf]: Render[Up, Singleton[E]] = RenderImpl[Up, Singleton[E]](Set(valueOf[E]))
  // cannot render Universum
  // cannot render Not[A <: UniSet] = Subtract[Universum, A]
  implicit def UnionRender[Up, A <: UniSet, B <: UniSet](implicit ra: Render[Up, A], rb: Render[Up, B]): Render[Up, Union[A,B]] = RenderImpl[Up, Union[A,B]](ra.elements ++ rb.elements)
  implicit def IntersectionRender[Up, A <: UniSet, B <: UniSet](implicit ra: Render[Up, A], rb: Render[Up, B]): Render[Up, Intersection[A,B]] = RenderImpl[Up, Intersection[A,B]](ra.elements.intersect(rb.elements))
  implicit def SubtractRender[Up, A <: UniSet, B <: UniSet](implicit ra: Render[Up, A], rb: Render[Up, B]): Render[Up, Subtract[A,B]] = RenderImpl[Up, Subtract[A,B]](ra.elements -- rb.elements)
  implicit def XorRender[Up, A <: UniSet, B <: UniSet](implicit ra: Render[Up, A], rb: Render[Up, B]): Render[Up, Xor[A,B]] = RenderImpl[Up, Xor[A,B]](ra.elements ++ rb.elements -- ra.elements.intersect(rb.elements))
  implicit def InsertRender[Up, E<:Up:ValueOf, S <: UniSet](implicit rs: Render[Up, S]): Render[Up, Insert[E, S]] = RenderImpl[Up, Insert[E, S]](rs.elements + valueOf[E])
}
sealed trait RenderMapLowPriority extends RenderLowPriority {
  implicit def MapEmptyRender[Up, F[_]]: Render[Up, UniMap[Empty, F]] = RenderImpl[Up, UniMap[Empty, F]](Set())
  implicit def MapSingletonRender[Up, E, F[_]](implicit me: ValueOf[F[E]], ev: F[E] <:< Up): Render[Up, UniMap[Singleton[E], F]] = RenderImpl[Up, UniMap[Singleton[E], F]](Set(valueOf[F[E]]))
  // cannot render Universum
  // cannot render Not[A <: UniSet] = Subtract[Universum, A]
  implicit def MapUnionRender[Up, A <: UniSet, B <: UniSet, F[_]](implicit ra: Render[Up, UniMap[A,F]], rb: Render[Up, UniMap[B,F]]): Render[Up, UniMap[Union[A,B], F]] = RenderImpl[Up, UniMap[Union[A,B], F]](ra.elements ++ rb.elements)
  implicit def MapIntersectionRender[Up, A <: UniSet, B <: UniSet, F[_]](implicit ra: Render[Up, UniMap[A,F]], rb: Render[Up, UniMap[B,F]]): Render[Up, UniMap[Intersection[A,B], F]] = RenderImpl[Up, UniMap[Intersection[A,B], F]](ra.elements.intersect(rb.elements))
  implicit def MapSubtractRender[Up, A <: UniSet, B <: UniSet, F[_]](implicit ra: Render[Up, UniMap[A,F]], rb: Render[Up, UniMap[B,F]]): Render[Up, UniMap[Subtract[A,B], F]] = RenderImpl[Up, UniMap[Subtract[A,B], F]](ra.elements -- rb.elements)
  implicit def MapXorRender[Up, A <: UniSet, B <: UniSet, F[_]](implicit ra: Render[Up, UniMap[A,F]], rb: Render[Up, UniMap[B,F]]): Render[Up, UniMap[Xor[A,B], F]] = RenderImpl[Up, UniMap[Xor[A,B], F]](ra.elements ++ rb.elements -- ra.elements.intersect(rb.elements))
  implicit def MapInsertRender[Up, E, S <: UniSet, F[_]](implicit rs: Render[Up, UniMap[S, F]], me: ValueOf[F[E]], ev: F[E] <:< Up): Render[Up, UniMap[Insert[E, S], F]] = RenderImpl[Up, UniMap[Insert[E, S], F]](rs.elements + valueOf[F[E]])
}
sealed trait BelongsToHighPriority extends BelongsToLowPriority {
  implicit def UnionBelongsToA[Element, A <: UniSet,B <: UniSet](implicit ea: BelongsTo[Element, A]): BelongsTo[Element, Union[A,B]] = new BelongsTo[Element, Union[A,B]] {}
  // if we insert element, then it belongs to the set.
  implicit def InsertEBelongsTo[Element, S <: UniSet]: BelongsTo[Element, Insert[Element, S]] = new BelongsTo[Element, Insert[Element, S]] {}
}

sealed trait EachElementIsSubtypeLowPriority extends UniProperties {
  implicit def EmptyEachElementIsSubtype[E]: EachElementIsSubtype[E, Empty] = new EachElementIsSubtype[E, Empty] {}
  implicit def SingletonEachElementIsSubtype[Up, E<:Up]: EachElementIsSubtype[Up, Singleton[E]] = new EachElementIsSubtype[Up, Singleton[E]] {}
  implicit def UnionEachElementIsSubtype[Up, A<: UniSet, B <: UniSet](implicit ea: EachElementIsSubtype[Up, A], eb: EachElementIsSubtype[Up, B]): EachElementIsSubtype[Up, Union[A,B]] =
    new EachElementIsSubtype[Up, Union[A,B]] {}

  implicit def IntersectionBEachElementIsSubtype[Up, A<: UniSet, B <: UniSet](implicit eb: EachElementIsSubtype[Up, B]): EachElementIsSubtype[Up, Intersection[A,B]] =
    new EachElementIsSubtype[Up, Intersection[A,B]] {}

  implicit def XorEachElementIsSubtype[Up, A<: UniSet, B <: UniSet](implicit ea: EachElementIsSubtype[Up, A], eb: EachElementIsSubtype[Up, B]): EachElementIsSubtype[Up, Xor[A,B]] =
    new EachElementIsSubtype[Up, Xor[A,B]] {}

  implicit def InsertEachElementIsSubtype[Up, E<:Up, S <: UniSet](implicit es: EachElementIsSubtype[Up, S]): EachElementIsSubtype[Up, Insert[E, S]] = new EachElementIsSubtype[Up, Insert[E, S]] {}
}
sealed trait EachElementIsSubtypeHighPriority extends EachElementIsSubtypeLowPriority {
  implicit def IntersectionAEachElementIsSubtype[Up, A<: UniSet, B <: UniSet](implicit ea: EachElementIsSubtype[Up, A]): EachElementIsSubtype[Up, Intersection[A,B]] =
    new EachElementIsSubtype[Up, Intersection[A,B]] {}

}

sealed trait EqualSets extends UniSetsBase with IsSubSetOfHighPriority {
  implicit def equalUnionAB[A<:UniSet, B<:UniSet]:Equal[Union[A, B], Union[B, A]] = new Equal[Union[A, B], Union[B, A]] {}
//  {
//    override def aToB(a: Union[A, B]): Union[B, A] = new Union[B, A]{}
//
//    override def bToA(b: Union[B, A]): Union[A, B] = ???
//  }
  implicit def equalUnionEmpty[S<:UniSet]:Equal[Union[S, Empty], S] = new Equal[Union[S, Empty], S] {}
  implicit def equalInsertUnion[E, S<:UniSet]:Equal[Insert[E, S],Union[Singleton[E], S]] = new Equal[Insert[E, S],Union[Singleton[E], S]]{}
  implicit def equalAB[A<:UniSet, B<:UniSet](implicit ab: IsSubSetOf[A,B], ba: IsSubSetOf[B, A]): Equal[A,B] = new Equal[A,B] {}

  implicit def insertExistingElement[E, S<:UniSet](implicit es: BelongsTo[E, S]): Equal[Insert[E, S], S] = new Equal[Insert[E, S], S] {}
  implicit def insertExistingElement2[E, S<:UniSet](implicit es: BelongsTo[E, S]): Equal[Union[Singleton[E], S], S] = new Equal[Union[Singleton[E], S], S] {}
  implicit def mapEq[A<:UniSet, B<:UniSet, F[_]](implicit eq: Equal[A, B]): Equal[UniMap[A, F], UniMap[B, F]] = new Equal[UniMap[A, F], UniMap[B, F]] {}
}

sealed trait SingletonSets extends UniProperties {
  implicit def singletonCardinality1[Up, E <: Up]: Cardinality1[Up, Singleton[E]]{type Element = E} = new Cardinality1[Up, Singleton[E]]{type Element = E}
  implicit def insert1Cardinality1[Up, E <: Up]: Cardinality1[Up, Insert[E, Empty]]{type Element = E} = new Cardinality1[Up, Insert[E, Empty]]{type Element = E}
}

object UniSets extends BelongsToHighPriority
  with IsSubSetOfHighPriority
  with EachElementIsSubtypeHighPriority
  with RenderLowPriority
  with RenderMapLowPriority
  with EqualSets
  with SingletonSets
