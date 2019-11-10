package ru.primetalk.contacts.core

trait TypeBasedSets {

//  type TypeBasedSet
  type Singleton[+E] = (E => Nothing) => Nothing
  type Not[-A] = A => Nothing
  type Union[+A, +B] = ((A => Nothing) with (B => Nothing)) => Nothing
  type Universum = Any
  type Empty = Nothing

}

object TypeBasedSets extends TypeBasedSets
