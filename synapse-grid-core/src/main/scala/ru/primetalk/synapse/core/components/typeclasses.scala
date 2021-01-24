package ru.primetalk.synapse.core.components

trait Show[T]:
  def show(t: T): String

type SimpleSignalProcessor = Signal0 => IterableOnce[Signal0]

trait ComponentStatelessInterpreter[C]:
  def runtimeStatelessInterpreter(c: C): SimpleSignalProcessor

trait Graphviz[A]:
  def toDot(a: A): String

/**
  * Named is used to store graph specific information - label or name - 
  * directly in the object itself.
  */
trait Named:
  def name: String

  override def toString: String =
    getClass.getSimpleName + "(\"" + name + "\")"

object Named:
  given Show[Named] = _.name
