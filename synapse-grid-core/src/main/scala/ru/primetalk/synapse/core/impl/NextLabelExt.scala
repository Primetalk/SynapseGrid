package ru.primetalk.synapse.core.impl

import ru.primetalk.synapse.core.{SystemBuilderExtensionId, SystemBuilderExtension, BasicSystemBuilder}
import scala.language.implicitConversions

/**
 * An extension that adds the collection of labels that can be used next.
 * @author zhizhelev, 25.03.15.
 */
trait NextLabelExt {
  /** An extension that adds easy labelling to System builder. */
  class LabellingExt(val sb: BasicSystemBuilder) extends SystemBuilderExtension {
    private[synapse] var proposedLabels = List[String]()

    /** Adds a few labels for subsequent links. */
    def labels(labels: String*) {
      sb.assertWritable()
      proposedLabels = labels.toList ::: proposedLabels
    }

    /** Gives the next label according to the following rules:
      * - use userProvidedLabel if the user mention it directly;
      * - looks for the next label in the list of labels previously set by user;
      * - constructs a new label using defaultLabel if there are no labels.
      */
    def nextLabel(userProvidedLabel: String, defaultLabel: => String): String = {

      (userProvidedLabel, proposedLabels) match {
        case ("", List()) ⇒ defaultLabel
        case ("", head :: tail) ⇒
          sb.assertWritable()
          proposedLabels = tail
          head
        case (label, _) => label
      }
    }
  }

  implicit val LabellingExtId = new SystemBuilderExtensionId(new LabellingExt(_))

  //(AuxContactNumberingExtId)
  implicit def sbToLabelling(sb: BasicSystemBuilder): LabellingExt = sb.extend[LabellingExt]


}
