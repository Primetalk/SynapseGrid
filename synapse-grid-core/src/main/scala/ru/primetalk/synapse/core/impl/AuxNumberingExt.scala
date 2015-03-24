package ru.primetalk.synapse.core.impl

import ru.primetalk.synapse.core._
import scala.language.implicitConversions

/**
 * An extension that adds counter for aux contacts.
 * @author zhizhelev, 25.03.15.
 */
trait AuxNumberingExt {
  class AuxContactNumberingExt(val sb: BasicSystemBuilder) extends SystemBuilderExtension {
    private var auxContactNumber = 0

    def nextContactName = {
      sb.assertWritable()
      auxContactNumber += 1
      "c" + (auxContactNumber - 1)
    }

    def auxContact[T] =
      new Contact[T](nextContactName, AuxiliaryContact)

  }

  implicit val AuxContactNumberingExtId = new SystemBuilderExtensionId(new AuxContactNumberingExt(_))
  implicit def sbToAux(sb: BasicSystemBuilder): AuxContactNumberingExt = sb.extend[AuxContactNumberingExt]

}
