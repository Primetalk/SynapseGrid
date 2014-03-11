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

import scala.language.existentials

sealed trait RuntimeComponent extends Named {
  def isStateful: Boolean
}

sealed trait RuntimeComponentTransparent extends RuntimeComponent {
  /**
   * the list of contacts that trigger processing of the component
   */
  val inputContacts: List[Contact[_]]
  /**
   * outputContacts - the list of contacts that can be influenced by this component.
   */
  val outputContacts: List[Contact[_]]
}

/**
 * The most popular runtime component. Transforms a signal into other signals.
 * This component is not only FlatMap link. It can represent almost any stateless part of a system.
 * @param f - the actual transformation
 */
case class RuntimeComponentFlatMap(
                                    name: String,
                                    input: Contact[_],
                                    output: Contact[_],
                                    f: Signal[_] =>
                                      List[Signal[_]]) extends RuntimeComponentTransparent {
  val inputContacts = List[Contact[_]](input)
  val outputContacts = List[Contact[_]](output)

  def isStateful: Boolean = false
}

/**
 * Is very similar to the most generic link — StateFlatMap.
 * This component refers a single stateHandle.
 */
case class RuntimeComponentStateFlatMap[S](
                                            name: String,
                                            inputContacts: List[Contact[_]],
                                            outputContacts: List[Contact[_]],
                                            stateHandle: Contact[S],
                                            f: (S, Signal[_]) //(state, signal)
                                              =>
                                              (S, List[Signal[_]]) //(state, signals)
                                            ) extends RuntimeComponentTransparent {
  def isStateful: Boolean = true
}

/**
 * The most general processing element. Can depend on a few states.
 */
case class RuntimeComponentMultiState(
                                       name: String,
                                       stateHandles: List[Contact[_]],
                                       f: (Context, Signal[_]) => TrellisElement) extends RuntimeComponent {
  def isStateful: Boolean = true
}

object RuntimeComponent {

  import SystemConvertingSupport._

  val linkToRuntimeComponent: SimpleComponentConverter = {
    case Link(from, to, FlatMapLink(f, name)) ⇒
      RuntimeComponentFlatMap(name, from, to, {
        (signal) ⇒
          val fun = f.asInstanceOf[Any ⇒ TraversableOnce[Any]]
          val res = fun(signal.data)
          res.map(new Signal(to, _)).toList
      })
    case Link(from, to, StatefulFlatMapLink(f, pe, name)) ⇒
      RuntimeComponentStateFlatMap[Any](name, List(from), List(to), pe, {
        (value, signal) ⇒
          val fun = f.asInstanceOf[(Any, Any) ⇒ (Any, Seq[Any])]
          val (nState, nDataSeq) = fun(value, signal.data)
          (nState, nDataSeq.toList.map(new Signal(to, _)))
      })
    case Link(from, to, NopLink(name)) ⇒
      RuntimeComponentFlatMap(name, from, to,
        (signal) ⇒ List(new Signal(to, signal.data))
      )
    case Link(from, to, StateZipLink(pe, name)) ⇒
      RuntimeComponentStateFlatMap[Any](name, List(from), List(to), pe, {
        (value, signal) ⇒
          (value, List(new Signal(to, (value, signal.data))))
      })
    case StateUpdate(from, pe, name, f) ⇒
      RuntimeComponentStateFlatMap[Any](name, List(from), List(), pe, {
        (value, signal) ⇒
          val result = f.asInstanceOf[(Any, Any) => Any](value, signal.data)
          (result, List())
      })
  }


}