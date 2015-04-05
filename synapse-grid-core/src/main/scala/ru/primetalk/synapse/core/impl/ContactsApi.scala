package ru.primetalk.synapse.core.impl

import ru.primetalk.synapse.core.{SystemBuilder, Contact}

/**
 * @author zhizhelev, 25.03.15.
 */
trait ContactsApi {

  def contact[T](name: String) = new Contact[T](name)

  def state[T](name:String, s0:T)(implicit sb:SystemBuilder) = sb.state(name, s0)

  def setSystemName(name:String)(implicit sb:SystemBuilder) = sb.setSystemName(name)
}

