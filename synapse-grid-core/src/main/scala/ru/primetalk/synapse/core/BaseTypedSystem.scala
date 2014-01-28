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
  protected val sb = new SystemBuilder {}
  sb.setSystemName(if(name == "") getClass.getSimpleName else name)
  protected def defineSystem() {}
  private lazy val system = {
    defineSystem()
    sb.toStaticSystem
  }
  def toStaticSystem = system
}
