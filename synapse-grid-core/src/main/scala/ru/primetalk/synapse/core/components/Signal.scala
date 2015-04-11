package ru.primetalk.synapse.core.components

/**
 * Signal is a pair of contact and data on it.
 * Two methods are provided to match those of pairs - _1 and _2.
 */
case class Signal[T](contact: Contact[T], data: T) {
  def _1 = contact

  def _2 = data
}
