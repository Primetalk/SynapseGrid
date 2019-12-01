package ru.primetalk.contacts.core

import org.scalatest.{FlatSpec, Matchers}
import UniSets._

class UnisetBelongsTest extends FlatSpec with Matchers{

  case object A
  type A = A.type
  case object B
  type B = B.type
  case object C
  type C = C.type
  case object D
  type D = C.type

  "BelongsTo" should "be order insensitive " in {
    type ab = Singleton[A] ∪ Singleton[B]
    type ba = Singleton[B] ∪ Singleton[A]
    implicitly[A ∊ ab]
    implicitly[A ∊ ba]
  }

  it should "work for first union element" in {
    type abc = Singleton[A] ∪ Singleton[B] ∪ Singleton[C]
    implicitly[A ∊ abc]
  }

  it should "work for middle union element" in {
    type abc = Singleton[A] ∪ Singleton[B] ∪ Singleton[C]
    implicitly[B ∊ abc]
  }

  it should "work for last union element" in {
    type abc = Singleton[A] ∪ Singleton[B] ∪ Singleton[C]
    implicitly[C ∊ abc]
  }

  it should "work for singleton" in {
    implicitly[A ∊ Singleton[A]]
  }

  it should "not be found for Empty" in {
    "implicitly[A ∊ Singleton[A]]" should compile
    "implicitly[A ∊ ∅]" shouldNot compile
  }

  it should "not found invalid implicit for singleton" in {
    "implicitly[A ∊ Singleton[A]]" should compile
    "implicitly[B ∊ Singleton[A]]" shouldNot compile
  }

  it should "not found invalid implicit for union" in {
    "implicitly[B ∊ (Singleton[A] ∪ Singleton[B])]" should compile
    "implicitly[C ∊ (Singleton[A] ∪ Singleton[B])]" shouldNot compile
  }

  it should "match empty union with singleton" in {
    type aEmpty = Empty ∪ Singleton[A]
    type emptyA =  Singleton[A] ∪ Empty
    implicitly[A ∊ aEmpty]
    implicitly[A ∊ emptyA]
  }

  it should "work for union intersection" in {
    type ab = Singleton[A] ∪ Singleton[B]
    type ca = Singleton[C] ∪ Singleton[A]
    type `ab∩ca` = ab ∩ ca
    implicitly[A ∊ `ab∩ca`]
    "implicitly[C ∊ `ab∩ba`]" shouldNot compile
  }

  it should "work for universum" in {
    implicitly[A ∊ Universum]
    implicitly[B ∊ Universum]
    implicitly[C ∊ Universum]
  }

  it should "works for tail Subtract" in {
    type abc = Singleton[A] ∪ Singleton[B] ∪ Singleton[C]
    type bc = Singleton[B] ∪ Singleton[C]
    type `abc_diff_bc` = abc Subtract bc
    implicitly[A ∊ `abc_diff_bc`]
    "implicitly[B ∊ `abc_diff_bc`]" shouldNot compile
    "implicitly[C ∊ `abc_diff_bc`]" shouldNot compile
  }

  it should "works for head Subtract" in {
    type abc = Singleton[A] ∪ Singleton[B] ∪ Singleton[C]
    type ab = Singleton[A] ∪ Singleton[B]
    type `abc_diff_ab` = abc Subtract ab
    implicitly[C ∊ `abc_diff_ab`]
    "implicitly[B ∊ `abc_diff_ab`]" shouldNot compile
    "implicitly[A ∊ `abc_diff_ab`]" shouldNot compile
  }

  it should "works for middle Subtract" in {
    type abcd = Singleton[A] ∪ Singleton[B] ∪ Singleton[C] ∪ Singleton[D]
    type bc = Singleton[B] ∪ Singleton[C]
    type `abcd_diff_bc` = abcd Subtract bc
    implicitly[A ∊ `abcd_diff_bc`]
    "implicitly[D ∊ `abcd_diff_bc`]" should compile
    "implicitly[B ∊ `abcd_diff_bc`]" shouldNot compile
    "implicitly[C ∊ `abcd_diff_bc`]" shouldNot compile
  }
}
