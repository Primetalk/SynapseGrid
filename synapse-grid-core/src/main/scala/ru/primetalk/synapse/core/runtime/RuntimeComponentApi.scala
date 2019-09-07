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
package ru.primetalk.synapse.core.runtime

import ru.primetalk.synapse.core.components.{Signal, _}

import scala.language.existentials

trait RuntimeComponentApi extends SignalsApi with TrellisApi {

  /** A runtime component that should be processed with pattern matching. */
  sealed trait RuntimeComponent extends Named {
    def isStateful: Boolean

    def toTotalTrellisProducer: TotalTrellisProducer
  }

  /** The trace of a signal towards the original one.
    * This class is intended for debug purposes.
    * It can also be used in pattern matching and back tracking algorithm.
    *
    * @param signalsReversed a list of signals starting from the last produced one and
    *                        collecting the signals that have lead to the production of the last signal.
    * @param processorsReversed a list of processors that have worked for the production of the current signal.
    *                           The length of the processors list is usually by one shorter than the length of the signals.
    *                           However if it is a "lost trace" (the one that didn't produce output), then the last processor
    *                           is added but the signal is not added. Thus the lengths are the same.
    */
  case class Trace(signalsReversed: List[Signal[_]], processorsReversed: List[RuntimeComponent] = Nil) {
    def this(signal: Signal[_]) = this(List(signal))

    def signal = signalsReversed.head
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

  case class BlackBoxRuntimeComponent(
    name: String,
    inputContacts: List[Contact[_]],
    outputContacts: List[Contact[_]],
    runtimeStatelessInterpreter: Signal[_] => Iterable[Signal[_]]
  ) extends RuntimeComponentTransparent {
    def isStateful: Boolean = false

    override def toTotalTrellisProducer: TotalTrellisProducer =
      (c, s) => (c, runtimeStatelessInterpreter(s))
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
                                        SignalCollection[Signal[_]]) extends RuntimeComponentTransparent {
    val inputContacts = List[Contact[_]](input)
    val outputContacts = List[Contact[_]](output)

    def isStateful: Boolean = false

    lazy val toTotalTrellisProducer: TotalTrellisProducer =
      (context, signal) => (context, f(signal))
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
                                                (S, SignalCollection[Signal[_]]) //(state, signals)
                                              ) extends RuntimeComponentTransparent {
    def isStateful: Boolean = true

    lazy val toTotalTrellisProducer: TotalTrellisProducer = {
      val fun = f.asInstanceOf[(Any, Signal[_]) => (Any, SignalCollection[Signal[_]])]
      (context, signal) => {
        val r = fun(context(stateHandle), signal)
        (context.updated(stateHandle, r._1), r._2)
      }
    }
  }

  /**
   * The most general processing element. Can depend on a few states.
   */
  case class RuntimeComponentMultiState(
                                         name: String,
                                         stateHandles: List[Contact[_]],
                                         f: TotalTrellisProducer) extends RuntimeComponent {
    def isStateful: Boolean = true

    override def toTotalTrellisProducer: TotalTrellisProducer = f
  }

  val linkToRuntimeComponent: PartialFunction[Component, RuntimeComponent] = {
    case Link(from, to, name, FlatMapLink(f)) ⇒
      RuntimeComponentFlatMap(name, from, to, {
        (signal) ⇒
          val fun = f.asInstanceOf[Any ⇒ TraversableOnce[Any]]
          val res = fun(signal.data)
          res.map(new Signal(to, _)).toList
      })
    case Link(from, to, name, NopLink()) ⇒
      RuntimeComponentFlatMap(name, from, to,
        (signal) ⇒ List(new Signal(to, signal.data))
      )
    case Link(from, to, name, StatefulFlatMapLink(f, pe)) ⇒
      RuntimeComponentStateFlatMap[Any](name, List(from), List(to), pe, {
        (value, signal) ⇒
          val fun = f.asInstanceOf[(Any, Any) ⇒ (Any, Seq[Any])]
          val (nState, nDataSeq) = fun(value, signal.data)
          (nState, nDataSeq.toList.map(new Signal(to, _)))
      })
    case Link(from, to, name, StateZipLink(pe)) ⇒
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
    case BlackBoxStatelessComponent(name, inputContacts, outputContacts, runtimeStatelessInterpreter) ⇒
      BlackBoxRuntimeComponent(name, inputContacts.toList, outputContacts.toList, runtimeStatelessInterpreter)
  }
}