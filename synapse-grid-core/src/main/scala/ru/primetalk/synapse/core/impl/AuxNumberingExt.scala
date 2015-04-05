package ru.primetalk.synapse.core.impl

import ru.primetalk.synapse.core.Contact

import scala.language.implicitConversions

/**
 * An extension that adds counter for aux contacts.
 * @author zhizhelev, 25.03.15.
 */
trait AuxNumberingExt extends ContactStyleExt {

  case object AuxiliaryContact extends ContactStyle

  class AuxContactNumberingExt(val sb: SystemBuilder) extends SystemBuilderExtension {
    private var auxContactNumber = 0

    def nextContactName = {
      sb.assertWritable()
      auxContactNumber += 1
      "c" + (auxContactNumber - 1)
    }

    def auxContact[T] =
      new Contact[T](nextContactName).styled(AuxiliaryContact)(sb)

  }

  implicit val AuxContactNumberingExtId = new SystemBuilderExtensionId[AuxContactNumberingExt](new AuxContactNumberingExt(_))
  implicit def sbToAux(sb: SystemBuilder): AuxContactNumberingExt = sb.extend[AuxContactNumberingExt]

  def auxContact[T](implicit sb:SystemBuilder) = sb.auxContact[T]
}
