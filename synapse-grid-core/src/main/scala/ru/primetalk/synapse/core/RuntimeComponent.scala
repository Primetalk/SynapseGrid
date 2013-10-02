///////////////////////////////////////////////////////////////
// СинаптическаяСеть
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 * (2-clause BSD license) See LICENSE
 *
 * Created: 21.09.2013
 */
package ru.primetalk.synapse.core

sealed trait RuntimeComponent extends Named

sealed trait RuntimeComponentTransparent extends RuntimeComponent {
  val inputContacts:List[Contact[_]]
  val outputContacts:List[Contact[_]]
}
/**
 * The most popular runtime component. Transforms a signal into other signals.
 * This component is not only FlatMap link. It can represent almost any stateless part of a system.
 * @param outputContacts - the list of contacts that can be influenced by this component.
 * @param f - the actual transformation
 */
case class RuntimeComponentFlatMap(
  name:String,
  inputContacts:List[Contact[_]],
  outputContacts:List[Contact[_]],
  f: Signal[_] =>
      List[Signal[_]]) extends RuntimeComponentTransparent

/**
  * Is very similar to the most generic link — StateFlatMap.
  * This component refers a single stateHandle.*/
case class RuntimeComponentStateFlatMap[S](
  name:String,
  inputContacts:List[Contact[_]],
  outputContacts:List[Contact[_]],
  stateHandle:Contact[S],
  f: (S, Signal[_]) //(state, signal)
    =>
    (S, List[Signal[_]])//(state, signals)
  ) extends RuntimeComponentTransparent

/**
 * The most general processing element.*/
case class RuntimeComponentMultiState(
  name:String,
  stateHandles:List[Contact[_]],
  f: (Context, Signal[_]) => TrellisElement) extends RuntimeComponent
