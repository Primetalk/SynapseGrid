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
 * Created: 12.05.14, zhizhelev
 */
package ru.primetalk.sinapse.frames

object SlotsOntology {

  import slots._

  case class SimpleSlotMeta[T](name: String) extends SlotId[T]

  def slot[T](name: String) = SimpleSlotMeta[T](name)

  val name = slot[String]("name")

}
