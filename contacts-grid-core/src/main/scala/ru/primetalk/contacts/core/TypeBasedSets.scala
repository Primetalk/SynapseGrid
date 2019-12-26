package ru.primetalk.contacts.core

@deprecated("Use UniSet", "26.12.2019")
trait TypeBasedSets {

//  type TypeBasedSet
  type Singleton[+E] = (E => Nothing) => Nothing
  type Not[-A] = A => Nothing
  type Union[+A, +B] = ((A => Nothing) with (B => Nothing)) => Nothing
  type Universum = Any
  type Empty = Nothing

}

@deprecated("Use UniSet", "26.12.2019")
object TypeBasedSets extends TypeBasedSets
