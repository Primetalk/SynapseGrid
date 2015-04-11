package ru.primetalk.synapse.core.dsl

import scala.language.implicitConversions

/**
 * @author zhizhelev, 25.03.15.
 */
trait BaseTypedSystemDsl extends SystemBuilderApi {
  /** A system builder with inputs and outputs given in advance.
    * */
  def systemBuilderTyped(name:String)(_inputs:Contact[_]*)(_outputs:Contact[_]*):SystemBuilder = {
    val res = new SystemBuilderC(name)
    res.inputs(_inputs:_*)
    res.outputs(_outputs:_*)
    res
  }

}
