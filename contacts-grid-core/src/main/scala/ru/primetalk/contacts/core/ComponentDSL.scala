package ru.primetalk.contacts.core

import UniSets._

trait ComponentDSL extends ComponentShapeBuilderAPI with MySignals {

  def liftIterableToComponent[A<:Contact:ValueOf, B<:Contact:ValueOf](f: A#Data => Iterable[B#Data]): Component[ComponentShape {
    type InputShape = Singleton[A]
    type OutputShape = Singleton[B]
  }] = {
    val lifted: Singleton[A] >> Singleton[B] = liftIterable(valueOf[A], valueOf[B])(f)
    val shape = InOutShape[A,B](valueOf[A], valueOf[B])
    createComponent(shape)(lifted)
  }

  def liftToComponent[A<:Contact:ValueOf, B<:Contact:ValueOf](f: A#Data => B#Data): Component[ComponentShape {
    type InputShape = Singleton[A]
    type OutputShape = Singleton[B]
  }] = {
    val lifted: Singleton[A] >> Singleton[B] = lift(valueOf[A], valueOf[B])(f)
    val shape = InOutShape[A,B](valueOf[A], valueOf[B])
    createComponent(shape)(lifted)
  }

}
