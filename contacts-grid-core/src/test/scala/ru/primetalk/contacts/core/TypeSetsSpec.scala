package ru.primetalk.contacts.core

import org.specs2.Specification
import org.specs2.matcher.MatchResult

class TypeSetsSpec extends Specification with TypeSets { def is = s2"""

  This is specification of TypedSet

  Everything happens at compile time.

  We can also "render" the set if elements are implicit
    - render empty sets should be the same set0EqSet0
    - render set1 and set11 should be the same tr
    - {a} == {a,a} ${ `{a} == {a,a}` }
    - {c,b,a} == ( {b,a} ∪ {c,b}) ${`{c,b,a} == ( {b,a} ∪ {c,b})`}
    - {a,b} == {b,a} ${`{a,b} == {b,a}`}
    - {b} == {b,a} ∩ {c,b} ${`{b} == {b,a} ∩ {c,b}`}
    - `{c,b,a} == {c,a,b}` ${`{c,b,a} == {c,a,b}`}
  """

  case object a
  type a = a.type
  case object b
  type b = b.type
  case object c
  type c = c.type

  val `{a}`: a ConsTypeSet ∅ = a +: Empty

  val `a ∊ {a}`: a ∊ ConsTypeSet[a, ∅] = BelongsTo.elementIsHeadOfTypeSet0[a, a, ∅]
  val `a ∊ {a} (2)`: a ∊ ConsTypeSet[a, ∅] = implicitly[a ∊ ConsTypeSet[a, ∅] ](`a ∊ {a}`)
  val `{a, a}`: ConsTypeSet[a, ∅] = a +: `{a}`

  val `{b,a}`: b +: a +: ∅ = b +: `{a}`
  val `{a,b}`: a +: b +: ∅ = a +: b +: Empty
  val `a ∊ (b +: a +: ∅)`: a ∊ (b +: a +: ∅) = implicitly[a ∊ (b +: a +: ∅)]
  def `{a} == {a,a}`: MatchResult[ConsTypeSet[a, ∅]] = `{a}` must be(`{a, a}`)

  def `{a,b} == {b,a}`: Boolean = `{a,b}` == (`{b,a}`)

  val `∀e | e ∊ {a} => e <: a`: EachElementIsSubtype[a, ConsTypeSet[a, ∅]] = implicitly[EachElementIsSubtype[a, a +: ∅]]
  val `∀e | e ∊ {a,a} => e <: a`: EachElementIsSubtype[a, ConsTypeSet[a, ConsTypeSet[a, ∅]]] = implicitly[EachElementIsSubtype[a, ConsTypeSet[a, ConsTypeSet[a, ∅]]]]

  def `{b}`: ConsTypeSet[b, ∅] = b +: Empty
  val `{c,b}`: ConsTypeSet[c, ConsTypeSet[b, ∅]] =  c +: `{b}`
  val `{c,b,a}`: ConsTypeSet[c, ConsTypeSet[b, ConsTypeSet[a, ∅]]] = c +: b +: a +: Empty
  val `{b,a} ∪ {c,b}` =  `{b,a}` ∪ `{c,b}`
  val `{b} ∩ {b}` = ∩(`{b}`, `{b}`)(IntersectionHelperConsContains[b, Empty, ConsTypeSet[b, ∅]])

  val `{c,b,a} =T= {c,a,b}` = implicitly[TypeSetEq[c +: b +: a +: ∅, c +: a +: b +: ∅]]

  def `{c,b,a} == {c,a,b}` = (`{c,b,a}`: c +: b +: a +: ∅) === (c +: a +: b +: Empty)

  def `{c,b,a} == ( {b,a} ∪ {c,b})` = `{c,b,a}` === (`{b,a}` ∪ `{c,b}`)

  val `{b,a} ∩ {c,b}`: IntersectionHelper[ConsTypeSet[b, ConsTypeSet[a, ∅]], ConsTypeSet[c, ConsTypeSet[b, ∅]]]#Out = `{b,a}` ∩`{c,b}`
  def `{b} == {b,a} ∩ {c,b}` = `{b}` === (`{b,a} ∩ {c,b}`)
  //val set123eq2 = implicitly[TypeSetEq[e3 +: e2 +: e1 +: ∅, e3 +: e1 +: ∅]]
  //val evAllE12 = implicitly[EachElementIsSubtype[e1.type, ConsTypeSet[e1.type, ConsTypeSet[e2.type, ∅]]]]

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
////  def set0EqSet0 = typeSet[∅] must be (typeSet[∅])
////  val set1 = typeSet[set1]
////  val set11 = typeSet[set11]
////  val tr: Boolean = set1 == set11
////  def set1EqSet11 = (set1: TypeSet) must be(set11: TypeSet)
//  val repr1 = TypeSetRepr.consRepr[e1.type, ∅]
//  val set1 = toList[set1](repr1)
//  val set11 = toList[set11]
//  def set1EqSet11 = set1 must be(set11)
}
