package ru.primetalk.synapse.core.impl

import ru.primetalk.synapse.core.{BasicSystemBuilder, DevNullContact, Contact}

/**
 * @author zhizhelev, 25.03.15.
 */
trait ContactsApi {

  def contact[T](name: String) = new Contact[T](name)

  def state[T](name:String, s0:T)(implicit sb:BasicSystemBuilder) = sb.state(name, s0)

  def setSystemName(name:String)(implicit sb:BasicSystemBuilder) = sb.setSystemName(name)
  /**
   * Special contact for consuming unnecessary data values.
   */
  lazy val devNull = new Contact[Any]("devNull", DevNullContact)

}
