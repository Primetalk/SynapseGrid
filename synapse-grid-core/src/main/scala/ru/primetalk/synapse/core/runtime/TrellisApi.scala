package ru.primetalk.synapse.core.runtime

import ru.primetalk.synapse.core.components.SignalsApi

/** Basic definitions for trellis processing*/
trait TrellisApi extends SignalsApi {//with RuntimeSystemApi {

  /** The simplest signal processor. Corresponds to FlatMap. */
  type SimpleSignalProcessor = Signal[_] => List[Signal[_]]

  /** The context for system is a map from state handles to values. */
  type Context = Map[Contact[_], _]

  type TrellisElement = (Context, List[Signal[_]])

  type ContextUpdater = List[(Contact[_], _)]

  type TrellisElementUpdater = (ContextUpdater, List[Signal[_]])

  def updateTrellisElement(te: TrellisElement, upd: TrellisElementUpdater): TrellisElement =
    ((te._1 /: upd._1.reverse)((ctx, u) => ctx + u), upd._2)

  /** A function that makes single(?) step over time. */
  type TrellisProducer = TrellisElement => TrellisElement
  /** A function that takes a single signal on input and returns the last trellis element. */
  type TotalTrellisProducer = ((Context, Signal[_]) => TrellisElement)


}
