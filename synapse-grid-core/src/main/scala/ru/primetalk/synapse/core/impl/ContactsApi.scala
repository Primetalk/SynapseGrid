package ru.primetalk.synapse.core.impl

import ru.primetalk.synapse.core._

/**
 * @author zhizhelev, 25.03.15.
 */
trait ContactsApi {

  def contact[T](name: String) = new Contact[T](name)

  def state[T](name:String, s0:T)(implicit sb:BasicSystemBuilder) = sb.state(name, s0)

  def setSystemName(name:String)(implicit sb:BasicSystemBuilder) = sb.setSystemName(name)
}

trait DevNullExt extends ContactStyleExt {

  case object DevNullContact extends ContactStyle

  class DevNullExtension(val sb: BasicSystemBuilder) extends SystemBuilderExtension{
    /**
     * Special contact for consuming unnecessary data values.
     */
    private[DevNullExt]
    lazy val devNull = new Contact[Any]("devNull").styled(DevNullContact)(sb)

  }
  implicit val DevNullExtId = new SystemBuilderExtensionId(new DevNullExtension(_))

  def devNull(implicit sb:BasicSystemBuilder):Contact[Any] =
    sb.extend(DevNullExtId).devNull

}