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
package ru.primetalk.synapse.core

abstract class BaseTypedSystem(name:String = "") {
  protected val sb = new SystemBuilderC(if(name == "") getClass.getSimpleName else name)

  protected def defineSystem(implicit sb:SystemBuilder) {}

  /**
   * Create contact and add it to the builder as an input
   */
  def input[T](name: String) =
    sb.input[T](name)

  /**
   * Create contact and add it to the builder as an output
   */
  def output[T](name: String) =
    sb.output[T](name)

  private lazy val system = {
    defineSystem(sb)
    sb.toStaticSystem
  }

  def toStaticSystem =
    system
}
