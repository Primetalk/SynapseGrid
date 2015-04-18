package ru.primetalk.synapse.core.dsl

import ru.primetalk.synapse.core.components.NopLink

/**
 * Some useful commands with implicit BasicSystemBuilder argument
 * @author zhizhelev, 29.03.15.
 */
trait SystemBuilderDsl2Api extends SystemBuilderDslApi{
  def connect[T1, T2 >: T1](c1: Contact[T1], c2: Contact[T2], name: String = "")(implicit sb:SystemBuilder) =
    c1 >> c2

  /** Declares the first contact as input and creates link to the second */
  def mappedInput[T, T2 >: T](c1: Contact[T], c2: Contact[T2])(implicit sb:SystemBuilder) =
    c1.inputMappedTo(c2)

  /** Declares the second contact as output and creates link from the first.
    * NB! Returns inner contact c1. */
  def mappedOutput[T, T2 >: T](c1: Contact[T], c2: Contact[T2])(implicit sb:SystemBuilder) = {
    c1.mapToOutput(c2)
    c1
  }

}
