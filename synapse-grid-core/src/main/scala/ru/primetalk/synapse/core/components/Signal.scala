package ru.primetalk.synapse.core.components

sealed abstract class Signal0:
  def _1: Contact0 = contact
  def contact: Contact0
  def data0: Any

/**
 * Signal is a pair of contact and data on it.
 * Two methods are provided to match those of pairs - _1 and _2.
 */
case class Signal[T](contact: Contact[T], data: T) extends Signal0:

  def data0: Any = data
  
  override def _1: Contact[T] = contact

  def _2: T = data
