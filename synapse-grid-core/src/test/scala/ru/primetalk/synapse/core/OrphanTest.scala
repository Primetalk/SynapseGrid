///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev
 *
 * Created: 28.10.13, zhizhelev
 */
package ru.primetalk.synapse.core

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class OrphanTest extends FunSuite {

  class DisconnectedSystemBuilder extends BaseTypedSystem{
    import sb._
    setSystemName("DisconnectedSystem")
    val i1 = input[Int]("i1")
    val c1 = contact[Int]("c1")
    val c2 = contact[Int]("c2")
    val o1 = output[Int]("o1")
    i1 >> c1
    c2 >> o1
  }
  test("orphan contacts"){
    val s = new DisconnectedSystemBuilder
    assert(orphanContactsRec(s) === List((".DisconnectedSystem", Set(s.c1, s.c2))))
  }
}
