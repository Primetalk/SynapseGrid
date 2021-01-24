package ru.primetalk.synapse.core.components

/** An outer description of a system.
  * Actual description is deferred to descendants.
  * See also [[Link]]s
  */
trait Component0 extends Named:
  def inputContacts: Set[Contact0]
  def outputContacts: Set[Contact0]


/** A component that has single input and single output.
  */
trait TwoPoleComponent[T1, T2] extends Component0:
  def from: Contact[T1]
  def to: Contact[T2]
  lazy val inputContacts : Set[Contact0] = Set(from)
  lazy val outputContacts : Set[Contact0] = Set(to)
