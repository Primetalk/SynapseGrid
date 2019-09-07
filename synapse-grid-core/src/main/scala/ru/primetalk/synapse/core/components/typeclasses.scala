package ru.primetalk.synapse.core.components

import simulacrum._

@typeclass trait Show[T] {
  def show(t: T): String
}

@typeclass trait ComponentStatelessInterpreter[C] {
  def runtimeStatelessInterpreter(c: C): Signal[_] => Iterable[Signal[_]]
}

@typeclass trait Graphviz[A] {
  def toDot(a: A): String
}
