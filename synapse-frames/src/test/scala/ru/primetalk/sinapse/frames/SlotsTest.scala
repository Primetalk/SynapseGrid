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

import org.scalatest.FunSuite

class SlotsTest extends FunSuite {

  import slots._
  import SlotsOntology._

  test("slots test") {
    val ps = name :: SNil
    val pv = name := "Vasya"
    val ei = pv :: EmptySlotSeqValueBuilder
    //    val i1 = pv :: ei
    //    val name = Prope
    //    val i1 =
  }

}
