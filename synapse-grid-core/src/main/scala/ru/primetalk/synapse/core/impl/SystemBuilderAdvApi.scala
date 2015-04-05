package ru.primetalk.synapse.core.impl

import ru.primetalk.synapse.core.{NopLink, Contact}

/**
 * Some useful commands with implicit BasicSystemBuilder argument
 * @author zhizhelev, 29.03.15.
 */
trait SystemBuilderAdvApi extends SystemBuilderApi with NextLabelExt{
  private[synapse] def nextLabel(userProvidedLabel: String, defaultLabel: => String)(implicit sb:SystemBuilder): String = {
    val lsb = sbToLabelling(sb)
    (userProvidedLabel, lsb.proposedLabels) match {
      case ("", List()) ⇒ defaultLabel
      case ("", head :: tail) ⇒
        sb.assertWritable()
        lsb.proposedLabels = tail
        head
      case (label, _) => label
    }
  }

  def connect[T1, T2 >: T1](c1: Contact[T1], c2: Contact[T2], name: String = "")(implicit sb:SystemBuilder) = {
    sb.addLink(c1, c2, name, new NopLink[T1, T2]())
    c2
  }

  /** Declares the first contact as input and creates link to the second */
  def mappedInput[T, T2 >: T](c1: Contact[T], c2: Contact[T2])(implicit sb:SystemBuilder) = {
    sb.inputs(c1)
    connect(c1, c2)
  }

  /** Declares the second contact as output and creates link from the first */
  def mappedOutput[T, T2 >: T](c1: Contact[T], c2: Contact[T2])(implicit sb:SystemBuilder) = {
    sb.outputs(c2)
    connect(c1, c2)
    c1
  }

}
