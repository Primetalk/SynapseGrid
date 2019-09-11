package ru.primetalk.synapse.core.dsl

import ru.primetalk.synapse.core.components.BlackBoxStatelessComponent

import scala.collection.mutable

/**
  * A newer version of API for creating "switcher". A switcher is a component with one input with pair of
  * contact and data and a few outputs of the same data type. It is implemented
  * as a low-level "component function" that operates at contacts level.
  *
  * @author zhizhelev, 06.09.19.
  */
trait Switcher2Dsl extends SystemBuilderDsl {

  class Switcher2Builder[T](c: Contact[Signal[T]], name: String = "")(implicit sb: SystemBuilder) {
    def fanOut(contacts: Contact[T]*): Unit = {
      val outputContacts: Set[Contact[_]] = contacts.toSet
      sb.addComponent(BlackBoxStatelessComponent(name, Set(c), outputContacts,
        {
          case Signal(_, s@Signal(contact, _)) if outputContacts.contains(contact) =>
            Iterable.single(s)
          case other => throw new IllegalStateException(s"Unexpected signal $other")
        }))
    }

  }
  implicit class SwitcherContactOps[T](val c: Contact[Signal[T]])(implicit sb: SystemBuilder) {
    def switcher(name: String = "") =
      new Switcher2Builder[T](c, name)(sb)
  }


}
