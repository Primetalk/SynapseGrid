package ru.primetalk.synapse.core.dsl

import ru.primetalk.synapse.core.subsystems.BaseTypedSystemDsl

/**
 * DSL for working with accumulators
 */
trait AccumulationDsl extends BaseTypedSystemDsl with SystemBuilderDsl {

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
      val seqOut = contact[List[T]](name + ".reversedOrder")
      trigger.labelNext(name + ".removeAll").withState(collection).
        stateMap{case (list, t)=> (Nil, list)} >>
        seqOut
    }

    /** Creates intermediate state and collects all incoming signals in direct order until some data appear on the trigger.
      * Then clears the state and returns collected data in a single list. */
    def collectUntil[TTrigger](trigger: Contact[TTrigger], name: String = c.name + "CCollector"): Contact[List[T]] = {
      val collection = sb.state[List[T]](name + "State", Nil)
      c.prependList(collection, name + ".collect")
      val seqOut = contact[List[T]](name + ".directOrder")
      trigger.labelNext(name + ".removeAll.reverse").withState(collection).
        stateMap{case (list, t)=> (Nil, list.reverse)} >>
      seqOut
    }

    /** JOINs two signals.
      *
      * Creates intermediate state and collects all incoming signals from the source contact
      * until some data appear on the trigger.
      * Then clears the state and returns collected data in a single list together with the trigger signal.
      * {{{
      *   in1.prependUntil(in2).map{ case (list1, data2) => ... }
      * }}}
     */
    def prependUntil[TTrigger](trigger:Contact[TTrigger], name: String = c.name + "PCollector"):Contact[(List[T], TTrigger)] = {
      val collection = sb.state[List[T]](name + "State", Nil)
      c.prependList(collection, name + ".collect")
      val seqOut = contact[(List[T],TTrigger)](name + ".reversedOrder")
      trigger.labelNext("(removeAll, _)").withState(collection).
        stateMap{case (list, t2) => (Nil, (list, t2))} >> seqOut
      seqOut
    }
    /**
     * Saves incoming signals to internal state.
     * The previous value is replaced with the new one.
     * When a signal appears on the trigger contact the current remembered value
     * {{{
     *   in1.lastJoinUntil(in2).map{ case (data1, data2) => }
     * }}}
     * if there were no input, then the trigger signal do not appear on the output
     * @param autoClear should clear internal state on trigger. Otherwise keeps the previous state.
     *
     * Prefer to use rightJoin.
     */

    def lastJoinUntil[TTrigger](trigger:Contact[TTrigger], name: String = c.name + "Collector", autoClear:Boolean = true):Contact[(T, TTrigger)] = {
      val collection = sb.state[Option[T]](name + "State", None)
      c.updateState(collection, name + ".collect"){ case (_, d)=>Some(d)}
      val seqOut = contact[(T,TTrigger)](name + ".last")
      (trigger.withState(collection) -> seqOut).stateFlatMap{
        case (stateOpt, t2) =>
          (if(autoClear) None else stateOpt, stateOpt.map(v => (v,t2)).toSeq)
      }
      seqOut
    }

    /** Creates a state for temporary storing the value
      * from the current contact.
      *
      * A kind of a latch.
      *
      * for all  trigger signals that  appear during `latch delay`
      * the join is successful.
      * If nothing has been remembered, then nothing appears on the output.
      *
      * The latched value is cleared automatically after the given delay.
      * This is better than `lastJoinUntil` when trigger do not appear in our timeslot. The
      * remembered value is automatically removed anyway.
      * */
    def rightJoin[TTrigger](trigger:Contact[TTrigger], name: String = c.name + "RightJoin", delay:Int = 1):Contact[(Option[T], TTrigger)] = {
      val collection = sb.state[Option[T]](name + "State", None)
      c.updateState(collection, name + ".collect"){ case (_, d)=>Some(d)}
      c.delay(delay).updateState(collection, name + ".collect"){ case (_, d)=>None}
      val seqOut = contact[(Option[T],TTrigger)](name + ".rightJoin")
      (trigger.labelNext("(savedOpt, _)").withState(collection) -> seqOut).stateMap{
        case (stateOpt, t2) =>
          (stateOpt, (stateOpt,t2))
      }
      seqOut
    }
  }

}
