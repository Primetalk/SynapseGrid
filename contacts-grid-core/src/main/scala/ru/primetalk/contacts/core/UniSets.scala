package ru.primetalk.contacts.core

import scala.annotation.implicitNotFound

// Here we define operations on sets irrespective of any evidences
sealed trait UniSetsBase {

  sealed trait UniSet

  sealed trait Empty extends UniSet
  type ∅ = Empty
  sealed trait Singleton[E] extends UniSet
  sealed trait Universum extends UniSet

  sealed trait Union[A <: UniSet, B <: UniSet]
  sealed trait Intersection[A <: UniSet,B <: UniSet] extends UniSet

  sealed trait Subtract[A <: UniSet,B <: UniSet] extends UniSet
  type Not[A <: UniSet] = Subtract[Universum, A] // Universum \ A

  sealed trait Xor[A <: UniSet,B <: UniSet] extends UniSet

  sealed trait Insert[E, S <: UniSet] extends UniSet
}

sealed trait BelongsToLowPriority extends UniSetsBase {
  // Here we define sets in a "non-constructive way". In particular, we
  // use "belongs to" relation as a primary definition of a set.
  // All other operations should define BelongsTo relation.


  // Public relation that can be inferred from operations on sets.
  @implicitNotFound("Couldn't prove that element belongs to set")
  sealed trait BelongsTo[Element, Set]

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

//  implicit def runtimeSingleton[E<: S]
}

sealed trait IsSubSetOfLowPriority extends UniSetsBase {
  @implicitNotFound("Couldn't prove that element belongs to set")
  sealed trait IsSubSetOf[A <: UniSet,B <: UniSet]
  type ⊂[A <: UniSet,B <: UniSet] = IsSubSetOf[A,B]
  type <=[A <: UniSet,B <: UniSet] = IsSubSetOf[A,B]
  // Implementation of BelongsTo using subsets
  type BelongsToViaSubset[E,S <: UniSet] = IsSubSetOf[Singleton[E], S]

  // See also reflectivity below for Empty <= Empty
  implicit def EmptyIsSubSetOf[A<:UniSet]: IsSubSetOf[Empty, A] = new IsSubSetOf[Empty, A] {}
  //
  implicit def UnionABIsSubsetOfS[A <: UniSet, B <: UniSet, S <: UniSet](implicit as: IsSubSetOf[A, S], bs: IsSubSetOf[B, S]): IsSubSetOf[Union[A,B], S] = new IsSubSetOf[Union[A,B], S] {}

  implicit def SIsSubsetOfUnionAB_A[S <: UniSet, A <: UniSet, B <: UniSet](implicit smabb: IsSubSetOf[Subtract[S, A], B]): IsSubSetOf[S, Union[A,B]] = new IsSubSetOf[S, Union[A,B]] {}

  //TODO  implicit def SIsSubsetOfUnionAB[S <: UniSet, A <: UniSet, B <: UniSet](implicit as: IsSubSetOf[A, S], bs: IsSubSetOf[B, S]): IsSubSetOf[Union[A,B], S] = new BelongsTo[Element, Union[A,B]] {}

  //  sealed trait Union[A <: UniSet, B <: UniSet]
//  sealed trait Intersection[A <: UniSet,B <: UniSet] <: UniSet

  implicit def SIsSubSetOfIntersectionAB[S <: UniSet, A <: UniSet, B <: UniSet](implicit sa: IsSubSetOf[S,A], sb: IsSubSetOf[S,B]): IsSubSetOf[S, Intersection[A,B]] = new IsSubSetOf[S, Intersection[A,B]] {}
  //
  implicit def SubIsSubsetOfA[A<:UniSet, B<:UniSet]: IsSubSetOf[Subtract[A,B], A] = new IsSubSetOf[Subtract[A,B], A] {}

  implicit def AIsSubSetOfXor[A<:UniSet, B<:UniSet]: IsSubSetOf[Subtract[A,B], Xor[A,B]] = new IsSubSetOf[Subtract[A,B], Xor[A,B]] {}
  implicit def BIsSubSetOfXor[A<:UniSet, B<:UniSet]: IsSubSetOf[Subtract[B,A], Xor[A,B]] = new IsSubSetOf[Subtract[B,A], Xor[A,B]] {}

  implicit def EIsSubSetOfInsert[E, S <: UniSet]: IsSubSetOf[Singleton[E], Insert[E,S]] = new IsSubSetOf[Singleton[E], Insert[E,S]] {}
  implicit def SIsSubSetOfInsert[E, S <: UniSet]: IsSubSetOf[S, Insert[E,S]] = new IsSubSetOf[S, Insert[E,S]] {}
}

sealed trait IsSubSetOfHighPriority extends IsSubSetOfLowPriority {
  implicit def ReflectivityIsSubSetOf[A <: UniSet]: IsSubSetOf[A, A] = new IsSubSetOf[A, A] {}
  implicit def TransitivityIsSubSetOf[A <: UniSet, B <: UniSet, C <: UniSet](implicit ab: IsSubSetOf[A,B], bc: IsSubSetOf[B,C]): IsSubSetOf[A, C] = new IsSubSetOf[A, C] {}
  //  implicit def SingletonIsSubSetOf[Element]: IsSubSetOf[Singleton[Element], Singleton[Element]] = new IsSubSetOf[Singleton[Element], Singleton[Element]] {}

  implicit def UniversumIsSuperSetOf[A<:UniSet]: IsSubSetOf[A, Universum] = new IsSubSetOf[A, Universum] {}
  implicit def SIsSubsetOfUnionAB_B[S <: UniSet, A <: UniSet, B <: UniSet](implicit smbba: IsSubSetOf[Subtract[S, B], A]): IsSubSetOf[S, Union[A,B]] = new IsSubSetOf[S, Union[A,B]] {}
  implicit def SubIsSubsetOfNotB[A<:UniSet, B<:UniSet]: IsSubSetOf[Subtract[A,B], Not[B]] = new IsSubSetOf[Subtract[A,B], Not[B]] {}

}
sealed trait RenderLowPriority extends UniSetsBase {
  sealed trait Render[A <: UniSet] {
    def elements: Set[Any]
  }

  implicit def EmptyRender: Render[Empty] = new Render[Empty] {
    override def elements: Set[Any] = Set()
  }
  implicit def SingletonRender[E:ValueOf]: Render[Singleton[E]] = new Render[Singleton[E]] {
    override def elements: Set[Any] = Set(implicitly[scala.ValueOf[E]].value)
  }
  // cannot render Universum
  // cannot render Not[A <: UniSet] = Subtract[Universum, A]
  implicit def UnionRender[A <: UniSet, B <: UniSet](implicit ra: Render[A], rb: Render[B]): Render[Union[A,B]] = new Render[Union[A,B]] {
    override def elements: Set[Any] = ra.elements ++ rb.elements
  }
  implicit def IntersectionRender[A <: UniSet, B <: UniSet](implicit ra: Render[A], rb: Render[B]): Render[Intersection[A,B]] = new Render[Intersection[A,B]] {
    override def elements: Set[Any] = ra.elements.intersect(rb.elements)
  }
  implicit def SubtractRender[A <: UniSet, B <: UniSet](implicit ra: Render[A], rb: Render[B]): Render[Subtract[A,B]] = new Render[Subtract[A,B]] {
    override def elements: Set[Any] = ra.elements -- rb.elements
  }
  implicit def XorRender[A <: UniSet, B <: UniSet](implicit ra: Render[A], rb: Render[B]): Render[Xor[A,B]] = new Render[Xor[A,B]] {
    override def elements: Set[Any] = ra.elements ++ rb.elements -- ra.elements.intersect(rb.elements)
  }
  implicit def InsertRender[E:ValueOf, S <: UniSet](implicit rs: Render[S]): Render[Insert[E, S]] = new Render[Insert[E, S]] {
    override def elements: Set[Any] = rs.elements + implicitly[ValueOf[E]].value
  }
}
sealed trait BelongsToHighPriority extends BelongsToLowPriority {
  implicit def UnionBelongsToA[Element, A <: UniSet,B <: UniSet](implicit ea: BelongsTo[Element, A]): BelongsTo[Element, Union[A,B]] = new BelongsTo[Element, Union[A,B]] {}
  // if we insert element, then it belongs to the set.
  implicit def InsertEBelongsTo[Element, S <: UniSet]: BelongsTo[Element, Insert[Element, S]] = new BelongsTo[Element, Insert[Element, S]] {}
}

object UniSets extends BelongsToHighPriority with IsSubSetOfHighPriority
