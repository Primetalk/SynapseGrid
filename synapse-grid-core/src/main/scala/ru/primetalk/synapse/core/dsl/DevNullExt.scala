package ru.primetalk.synapse.core.dsl

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
    lazy val devNull = contact[Any]("devNull").styled(DevNullContact)(sb)

  }
  implicit val DevNullExtId:SystemBuilderExtensionId[DevNullExtension] = new SystemBuilderExtensionId[DevNullExtension](new DevNullExtension(_))

  /**
   * Special contact for consuming unnecessary data values.
   * It is often used as a sink contact in foreach and exec commands.
   */
  def devNull(implicit sb:SystemBuilder):Contact[Any] =
    sb.extend(DevNullExtId).devNull

}
