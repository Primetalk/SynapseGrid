package ru.primetalk.contacts.core

import scala.annotation.implicitNotFound

sealed trait UniSetsLowPriority {

  // Here we define sets in a "non-constructive way". In particular, we
  // use "belongs to" relation as a primary definition of a set.
  // All other operations should define BelongsTo relation.

  // Public relation that can be inferred from operations on sets.
  @implicitNotFound("Couldn't prove that element belongs to set")
  sealed trait BelongsTo[Element, Set]

  sealed trait UniSet

  sealed trait Empty
  // there is no instance of BelongsTo for Empty
  sealed trait Singleton[E]
  implicit def SingletonBelongsTo[Element]: BelongsTo[Element, Singleton[Element]] = new BelongsTo[Element, Singleton[Element]] {}

  sealed trait Union[A,B]
  implicit def UnionBelongsToB[Element, A,B](implicit eb: BelongsTo[Element, B]): BelongsTo[Element, Union[A,B]] = new BelongsTo[Element, Union[A,B]] {}
  // see also high priority

  sealed trait Intersection[A,B]
  implicit def IntersectionBelongsTo[Element, A,B](implicit ea: BelongsTo[Element, A], eb: BelongsTo[Element, B]): BelongsTo[Element, Intersection[A,B]] = new BelongsTo[Element, Intersection[A,B]] {}

  sealed trait Universum
  implicit def universum[E]: BelongsTo[E, Universum] = new BelongsTo[E, Universum] {}


  sealed trait Subtract[A,B]
  type Not[A] = Subtract[Universum, A] // Universum \ A
  implicit def SubtractBelongsToA[E, A, B](implicit ea: BelongsTo[E, A]): BelongsTo[E,Subtract[A,B]] = new BelongsTo[E,Subtract[A,B]] {}
  implicit def SubtractBelongsToAB[E, A, B](implicit ea: BelongsTo[E, A], eb: BelongsTo[E,B]): BelongsTo[E,Subtract[A,B]] = ???

  sealed trait Xor[A,B]

  implicit def XorBelongsToA[E, A, B](implicit ea: BelongsTo[E, A]): BelongsTo[E,Xor[A,B]] = new BelongsTo[E,Xor[A,B]] {}
  implicit def XorBelongsToB[E, A, B](implicit eb: BelongsTo[E, B]): BelongsTo[E,Xor[A,B]] = new BelongsTo[E,Xor[A,B]] {}

  sealed trait Insert[E, S]
  // checking S has lower priority.
  // See also InsertEBelongsTo
  implicit def InsertSBelongsTo[Element, S](implicit s: BelongsTo[Element, S]): BelongsTo[Element, Insert[Element, S]] =
    new BelongsTo[Element, Insert[Element, S]] {}

  // produce runtime evidence when needed
  def runtimeBelongsTo[A, B<:A](b: B, s: Set[A]): Option[BelongsTo[B, Set[A]]] =
    if(s.contains(b)) Some(new BelongsTo[B, Set[A]] {})
    else None
}

sealed trait UniSetsHighPriority extends UniSetsLowPriority {
  implicit def UnionBelongsToA[Element, A,B](implicit ea: BelongsTo[Element, A]): BelongsTo[Element, Union[A,B]] =
    new BelongsTo[Element, Union[A,B]] {}
  // if we insert element, then it belongs to the set.
  implicit def InsertEBelongsTo[Element, S]: BelongsTo[Element, Insert[Element, S]] =
    new BelongsTo[Element, Insert[Element, S]] {}
}

object UniSets extends UniSetsHighPriority
