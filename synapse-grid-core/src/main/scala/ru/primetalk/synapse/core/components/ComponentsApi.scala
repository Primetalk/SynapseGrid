package ru.primetalk.synapse.core.components

/**
 * Import components to core package.
 * @author zhizhelev, 11.04.15.
 */
trait ComponentsApi {

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

  type Signal[T] = ru.primetalk.synapse.core.components.Signal[T]
  val Signal = ru.primetalk.synapse.core.components.Signal

  type SignalDist = ru.primetalk.synapse.core.components.SignalDist
  val SignalDist = ru.primetalk.synapse.core.components.SignalDist

}
