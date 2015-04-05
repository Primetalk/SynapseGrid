package ru.primetalk.synapse.core.impl

import ru.primetalk.synapse.core.Contact

/**
 * @author zhizhelev, 25.03.15.
 */
trait ContactsApi {

  def contact[T](name: String) = new Contact[T](name)

}

