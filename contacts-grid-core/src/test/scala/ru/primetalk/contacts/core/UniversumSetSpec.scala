package ru.primetalk.contacts.core

import org.specs2.Specification
import UniSets._

class UniversumSetSpec extends Specification { def is = s2"""

  """
  case object a
  type a = a.type
  case object b
  type b = b.type
  case object c
  type c = c.type

  type `{a}` = Singleton[a]
  type `{b}` = Singleton[b]
  type `{a,b}` = Union[Singleton[b], Singleton[a]]
  type `{b,c}` = Union[Singleton[b], Singleton[c]]
  type `{a,b,c}` = Union[Singleton[a], Union[Singleton[b], Singleton[c]]]
  type `Not{a}` = Subtract[Universum, `{a}`]
  // val `{a}`: Singleton[a] =
  val `a ∊ {a}`: BelongsTo[a, `{a}`] = implicitly[BelongsTo[a, `{a}`]]
  val `a ∊ {a,b}`: BelongsTo[a, Union[Singleton[b], Singleton[a]]] = implicitly[BelongsTo[a, Union[Singleton[b], Singleton[a]]]]
  val `c ∊ {a,b,c}`: BelongsTo[c, `{a,b,c}`] = implicitly[BelongsTo[c, `{a,b,c}`]]
//  val `a ∊ Not{a}` = implicitly[BelongsTo[a, Not[`{a}`]]

  val `c ∊ {a,b,c}\\{a,b}` = implicitly[BelongsTo[c, Subtract[`{a,b,c}`, `{a,b}`]]]

//  val `b ∊ {a,b} ^ {b,c}` = implicitly[BelongsTo[b, Xor[`{a,b}`, `{b,c}`]]]
//  val `b ∊ {a,b} ^ {b,c}` = implicitly[BelongsTo[Int, Xor[`{a,b}`, `{b,c}`]]]
}
