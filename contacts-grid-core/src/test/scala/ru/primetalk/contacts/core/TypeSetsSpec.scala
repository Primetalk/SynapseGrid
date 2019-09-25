package ru.primetalk.contacts.core

import org.specs2.Specification

class TypeSetsSpec extends Specification with TypeSets { def is = s2"""

  This is specification of TypedSet

  Everything happens at compile time.

  We can also "render" the set if elements are implicit
    - render empty sets should be the same set0EqSet0
    - render set1 and set11 should be the same tr
    - set1 and set11 should be the same $set1eqSet11
  """

  implicit case object e1
  type e1 = e1.type
  implicit case object e2
  type e2 = e2.type
  implicit case object e3
  type e3 = e3.type

  val set1: ConsTypeSet[e1.type, Empty.type] = addElement(e1, Empty)

  val bel: e1 ∊ ConsTypeSet[e1, Empty.type] = BelongsTo.elementIsHeadOfTypeSet0[e1, Empty.type]
  val evBelongs = implicitly[e1 ∊ ConsTypeSet[e1.type, Empty.type] ](bel)
  val set11: ConsTypeSet[e1.type, Empty.type] = addElement(e1, set1)

  val set21: e2 +: e1 +: ∅ = addElement(e2, set11)
  val bel1 = implicitly[e1 ∊ (e2 +: e1 +: ∅)]
  def set1eqSet11 = set1 must be(set11)


  val evAllE1 = implicitly[EachElementIsSubtype[e1.type, ConsTypeSet[e1.type, Empty.type]]]
  val evAllE11 = implicitly[EachElementIsSubtype[e1.type, ConsTypeSet[e1.type, ConsTypeSet[e1.type, Empty.type]]]]
  //val evAllE12 = implicitly[EachElementIsSubtype[e1.type, ConsTypeSet[e1.type, ConsTypeSet[e2.type, Empty.type]]]]

//  val evSet11eqSet1 = implicitly[set1.type =:= set11.type ]
  //val myEl2 = getEv[e1, myEl.type]
//
//  type set1 = e1 +: ∅
//
//  val evSet1 = implicitly[+:[e1,∅] =:= ConsTypeSet[e1,∅]]
//  val evSet11 = implicitly[+:[e1,∅] =:= +:[e1,+:[e1,∅]]]
//
//  type set12 = e2 +: e1 +: ∅
//
//  type set3 = e3 +: ∅
//
//  type set123 = e3 +: set12
//
//  type union = set12 ∪ set3
//
//  val ev = AddElementHelperAddElementToTypeSetWhenEBelongsToS[e1, e1 ConsTypeSet ∅]
//  val ev2 = AddElementHelperAddElementToTypeSet[e2, e1 +: ∅]
//
//  type set11 = e1 +: set1
//  val ev3 = implicitly[e1 ∊ set1]
//  val ev4 = implicitly[e1 ∊ set11]
//  val ev5 = implicitly[e1 ∊ set123]
//  // val ev6 = implicitly[e1 ∊ set3] // do not compile
////  assert(typeSet[set1]  == typeSet[set11], "should be the same")
//  val ev6 = implicitly[set1 =:= set1]
// // val ev7 = implicitly[set11 =:= set1]
//  //val addingTheSameElementEqualsSet12 = implicitly[set1 =:= set11]
//  //val unionEqualsSet3 = implicitly[set123 =:= union]//(UnionHelper.caseAHeadTail[e3, set1, union])
////  def set0EqSet0 = typeSet[Empty.type] must be (typeSet[∅])
////  val set1 = typeSet[set1]
////  val set11 = typeSet[set11]
////  val tr: Boolean = set1 == set11
////  def set1EqSet11 = (set1: TypeSet) must be(set11: TypeSet)
//  val repr1 = TypeSetRepr.consRepr[e1.type, Empty]
//  val set1 = toList[set1](repr1)
//  val set11 = toList[set11]
//  def set1EqSet11 = set1 must be(set11)
}
