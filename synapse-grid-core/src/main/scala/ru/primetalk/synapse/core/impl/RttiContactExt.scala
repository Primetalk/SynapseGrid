package ru.primetalk.synapse.core.impl

import ru.primetalk.synapse.core.Contact

/**
 * @author zhizhelev, 05.04.15.
 */
trait RttiContactExt {
  /** Contact with runtime type information.
    * Can be used for static analysis of the system. At least checking Nop links to connect compatible contacts.
    */
  //TODO: move type information to extension
  class RttiContact[T](name1: String = null)(implicit val classTag:scala.reflect.ClassTag[T]) extends Contact[T](name1)

}
