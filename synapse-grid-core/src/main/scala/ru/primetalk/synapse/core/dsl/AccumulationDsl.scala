package ru.primetalk.synapse.core.dsl

import ru.primetalk.synapse.core.subsystems.BaseTypedSystemDsl

/**
 * DSL for working with accumulators
 */
trait AccumulationDsl extends BaseTypedSystemDsl with SystemBuilderDslApi {

  /**
   * Collects all inputs until some other contact not issue control
   * command to pass through all collected data.
   */
  def collector[T, TTrigger](name: String, trigger: Contact[TTrigger])(implicit sb:SystemBuilder): (Contact[T], Contact[List[T]]) = {
    val in = contact[T](name)
    val seqOut = contact[List[T]](name + "List")
    val collection = sb.state[List[T]](name, Nil)
    new LinkBuilderOps(trigger -> seqOut)(sb).stateMap(collection, "trigger") {
      (s: List[T], input: TTrigger) ⇒
        val s2 = Nil: List[T]
        (s2, s)
    }

    (in, seqOut)
  }

  implicit class CollectingContact[T](c: Contact[T])(implicit sb:SystemBuilder) {
    /** Creates intermediate state and collects all incoming signals in reversed order until some data appear on the trigger.
      * Then clears the state and returns collected data in a single list. */
    def collectReversedUntil[TTrigger](trigger: Contact[TTrigger], name: String = c.name + "ReversedCollector"): Contact[List[T]] = {
      val collection = sb.state[List[T]](name + "State", Nil)
      c.prependList(collection, name + ".collectReversed")
      val seqOut = trigger.getState(collection, name + ".read")
      trigger.delay(1).clearList(collection, name + ".clear")
      seqOut
    }

    /** Creates intermediate state and collects all incoming signals in direct order until some data appear on the trigger.
      * Then clears the state and returns collected data in a single list. */
    def collectUntil[TTrigger](trigger: Contact[TTrigger], name: String = c.name + "Collector"): Contact[List[T]] = {
      val collection = sb.state[List[T]](name + "State", Nil)
      c.prependList(collection, name + ".collect")
      val seqOut = contact[List[T]](name + ".directOrder")
      (trigger.getState(collection, name + ".read") -> seqOut).labelNext("reverseStack").map(_.reverse)
      trigger.delay(1).clearList(collection, name + ".clear")
      seqOut
    }
  }

}
