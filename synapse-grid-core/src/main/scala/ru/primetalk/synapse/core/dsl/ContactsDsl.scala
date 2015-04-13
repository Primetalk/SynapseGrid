package ru.primetalk.synapse.core.dsl

import ru.primetalk.synapse.core.components.ComponentsApi

/**
 * @author zhizhelev, 25.03.15.
 */
trait ContactsDsl extends ComponentsApi {

  /** Create a contact with the given name.*/
  def contact[T](name: String) = new Contact[T](name)

}

