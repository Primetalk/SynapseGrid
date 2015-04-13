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

  type Link[T1, T2, -TL1 >: T1, +TL2 <: T2] = ru.primetalk.synapse.core.components.Link[T1, T2, TL1, TL2]
  val Link = ru.primetalk.synapse.core.components.Link

  type LinkInfo[-T1, +T2] = ru.primetalk.synapse.core.components.LinkInfo[T1, T2]

  type FlatMapLink[-T1, +T2] = ru.primetalk.synapse.core.components.FlatMapLink[T1, T2]
  val FlatMapLink = ru.primetalk.synapse.core.components.FlatMapLink

  type StatefulFlatMapLink[S, -T1, +T2] = ru.primetalk.synapse.core.components.StatefulFlatMapLink[S, T1, T2]
  val StatefulFlatMapLink = ru.primetalk.synapse.core.components.StatefulFlatMapLink

  type StateZipLink[S, -T1, +T2 >: T1] = ru.primetalk.synapse.core.components.StateZipLink[S, T1, T2]
  val StateZipLink = ru.primetalk.synapse.core.components.StateZipLink

  type NopLink[-T1, +T2 >: T1] = ru.primetalk.synapse.core.components.NopLink[T1, T2]
  val NopLink = ru.primetalk.synapse.core.components.NopLink

  type RedMapLink[-T1, +T2] = ru.primetalk.synapse.core.components.RedMapLink[T1, T2]
  val RedMapLink = ru.primetalk.synapse.core.components.RedMapLink



}
