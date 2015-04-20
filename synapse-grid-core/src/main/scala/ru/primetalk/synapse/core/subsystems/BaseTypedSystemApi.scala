///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 *
 * Created: 07.09.13, zhizhelev
 */
package ru.primetalk.synapse.core.subsystems

import ru.primetalk.synapse.core.dsl.SystemBuilderApi

trait BaseTypedSystemApi extends SystemBuilderApi with BaseTypedSystemDsl {

  /** A very convenient way to define a StaticSystem is by implementing BaseTypedSystem.
    *
    * Why is it so?
    * We define both the outer world interface (inputs and outputs) and implementation
    * (in defineSystem). Both are declared altogether, in the same class.
    *
    * However there is another way for system's construction, which provides
    * better decomposition. It is described in comment to TypedSystemConstructor
    */
  abstract class BaseTypedSystem(val name: String = "") extends WithStaticSystem {
    protected implicit val sb: SystemBuilder = new SystemBuilderC(if (name == "") getClass.getSimpleName else name)

    protected def defineSystem(implicit sb: SystemBuilder) {}

    /**
     * Create contact and add it to the builder as an input
     */
    protected
    def input[T](name: String) =
      sb.input[T](name)

    /**
     * Create contact and add it to the builder as an output
     */
    protected
    def output[T](name: String) =
      sb.output[T](name)

    private lazy val system = {
      defineSystem(sb)
      sb.toStaticSystem
    }

    def toStaticSystem =
      system

    def toTypedSystem:TypedSystem[this.type] = TypedSystem[this.type](this, toStaticSystem)
  }

  abstract class AbstractTypedSystem(val name: String) {
    protected
    def input[T](internalName: String): Contact[T] = contact[T](name + "." + internalName)

    protected
    def output[T](internalName: String): Contact[T] = contact[T](name + "." + internalName)
  }

}