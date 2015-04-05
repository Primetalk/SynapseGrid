package ru.primetalk.synapse.core.impl

import ru.primetalk.synapse.core.{SystemBuilderExtensionId, SystemBuilderExtension, SystemBuilder, Contact}
/**
 * @author zhizhelev, 05.04.15.
 */
trait DevNullExt extends ContactStyleExt {

  case object DevNullContact extends ContactStyle

  class DevNullExtension(val sb: SystemBuilder) extends SystemBuilderExtension{
    /**
     * Special contact for consuming unnecessary data values.
     */
    private[DevNullExt]
    lazy val devNull = new Contact[Any]("devNull").styled(DevNullContact)(sb)

  }
  implicit val DevNullExtId = new SystemBuilderExtensionId(new DevNullExtension(_))

  /**
   * Special contact for consuming unnecessary data values.
   * It is often used as a sink contact in foreach and exec commands.
   */
  def devNull(implicit sb:SystemBuilder):Contact[Any] =
    sb.extend(DevNullExtId).devNull

}
