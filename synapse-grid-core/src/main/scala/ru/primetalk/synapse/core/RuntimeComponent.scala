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

/**
 * The most general processing element.
 * Is very similar to the most generic link — StateFlatMap. */
sealed trait RuntimeComponent

case class RuntimeComponentLightweight(outputContacts:List[Contact[_]],
  f: Signal[_] =>
      List[Signal[_]]) extends RuntimeComponent

case class RuntimeComponentStateful[S](outputContacts:List[Contact[_]],
  stateHandle:Contact[S],
  f: (S, Signal[_]) //(state, signal)
    =>
    (S, List[Signal[_]])//(state, signals)
                                     ) extends RuntimeComponent
/** The most general processing element.
  * Is very similar to the most generic link — StateFlatMap. */
case class RuntimeComponentHeavy(
  stateHandles:List[Contact[_]],
  f: (Context, Signal[_]) => TrellisElement) extends RuntimeComponent
