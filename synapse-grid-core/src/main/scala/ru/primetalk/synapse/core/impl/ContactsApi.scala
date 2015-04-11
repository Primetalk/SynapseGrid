package ru.primetalk.synapse.core.impl

/**
 * @author zhizhelev, 25.03.15.
 */
trait ContactsApi {

  type Contact[T] = ru.primetalk.synapse.core.components.Contact[T]
  val Contact = ru.primetalk.synapse.core.components.Contact

  type StateHandle[T] = ru.primetalk.synapse.core.components.StateHandle[T]
  val StateHandle = ru.primetalk.synapse.core.components.StateHandle

  type StaticSystem = ru.primetalk.synapse.core.components.StaticSystem
  val StaticSystem = ru.primetalk.synapse.core.components.StaticSystem

  type Component = ru.primetalk.synapse.core.components.Component

  type ComponentWithInternalStructure = ru.primetalk.synapse.core.components.ComponentWithInternalStructure

  type Named = ru.primetalk.synapse.core.components.Named

  type StaticSystemExtensionId[+T] = ru.primetalk.synapse.core.components.StaticSystemExtensionId[T]

  def contact[T](name: String) = new Contact[T](name)

}

