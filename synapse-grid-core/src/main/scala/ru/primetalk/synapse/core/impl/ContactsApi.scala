package ru.primetalk.synapse.core.impl

/**
 * @author zhizhelev, 25.03.15.
 */
trait ContactsApi extends ComponentsApi {

  /** Create a contact with the given name.*/
  def contact[T](name: String) = new Contact[T](name)

}

