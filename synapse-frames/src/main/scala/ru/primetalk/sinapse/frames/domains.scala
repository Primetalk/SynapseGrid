///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2014                                   //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2014.
 * All rights reserved.
 * Authors: A.Zhizhelev
 *
 * Created: 25.04.14, zhizhelev
 */
package ru.primetalk.sinapse.frames

import ru.primetalk.sinapse.frames.properties._

object domains {

  case class SimpleDbDomain[T](dbType: String) extends Domain[T]

  def domain[T](dbType: String): Domain[T] = SimpleDbDomain(dbType)

  val text = domain[String]("text")

  case class SimplePropertyMeta[T, D1 <: Domain[T]](name: String, domain: D1) extends PropertyId {
    type Type = T
    type D = D1
  }

  def property[T, D1 <: Domain[T]](name: String, domain: D1): PropertyId = SimplePropertyMeta[T, D1](name: String, domain: D1)

  val name = property[String, text.type]("name", text)
}
