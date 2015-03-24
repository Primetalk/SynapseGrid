package ru.primetalk.synapse.core.impl

import ru.primetalk.synapse.core.{DevNullContact, Contact}

/**
 * @author zhizhelev, 25.03.15.
 */
trait ContactsApi {

  def contact[T](name: String) = new Contact[T](name)

  /**
   * Special contact for consuming unnecessary data values.
   */
  lazy val devNull = new Contact[Any]("devNull", DevNullContact)

}
