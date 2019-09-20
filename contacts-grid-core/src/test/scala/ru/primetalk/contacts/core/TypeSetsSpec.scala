package ru.primetalk.contacts.core

import org.specs2.Specification

class TypeSetsSpec extends Specification with TypeSets { def is = s2"""

  This is specification of TypedSet

  Everything happens at compile time.
  """

  object e1
  type e1 = e1.type
  object e2
  type e2 = e2.type
  object e3
  type e3 = e3.type

  type set1 = e2 + e1 + ∅

  type set2 = e3 + ∅

  type set3 = e3 + e2 + e1 + ∅

  type union = set1 ∪ set2

  val unionEqualsSet3 = implicitly[set3 =:= union]
}
