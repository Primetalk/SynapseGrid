package ru.primetalk.contacts.core

@deprecated("Use Signals", "26.12.2019")
trait SimpleSignals {

  case class Contact[T](data: T)

  case class Signal[T](contact: Contact[T], data: T)

  // We cannot express the fact that contact is in contact set in the type of signal processor.
  type SignalProcessor = Signal[_] => Iterable[Signal[_]]
}
