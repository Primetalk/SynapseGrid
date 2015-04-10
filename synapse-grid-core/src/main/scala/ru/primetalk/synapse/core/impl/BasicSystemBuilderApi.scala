package ru.primetalk.synapse.core.impl

import ru.primetalk.synapse.core.Contact

import scala.language.implicitConversions

/**
 * @author zhizhelev, 25.03.15.
 */
trait BasicSystemBuilderApi extends SystemBuilderApi {
  /** A system builder with inputs and outputs given in advance.
    * */
  def systemBuilderTyped(name:String)(_inputs:Contact[_]*)(_outputs:Contact[_]*):SystemBuilder = {
    val res = new SystemBuilderC(name)
    res.inputs(_inputs:_*)
    res.outputs(_outputs:_*)
    res
  }

  /** Automatic usage of extensions when an implicit extension id is present in the scope.*/
  implicit def implicitExtendBasicSystemBuilder[T <: SystemBuilderExtension](sb: SystemBuilder)(
    implicit extensionInstanceId: SystemBuilderExtensionId[T]): T =
    sb.extend(extensionInstanceId)

}
