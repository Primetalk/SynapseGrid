package ru.primetalk.synapse.core.components

/**
 * Stateful elements of the system.
 */
trait Stateful[State]:
  type StateType = State
  /**
   * The initial state of the element.
   */
  def s0: State
end Stateful
