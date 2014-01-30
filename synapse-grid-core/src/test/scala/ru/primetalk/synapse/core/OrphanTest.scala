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

  class DisconnectedSystemBuilder extends BaseTypedSystem("DisconnectedSystem"){
    val i1 = input[Int]("i1")
    val c1 = contact[Int]("c1")
    val c2 = contact[Int]("c2")
    val o1 = output[Int]("o1")

    override protected def defineSystem(implicit sb: SystemBuilder) = {
      i1 >> c1
      c2 >> o1
    }
  }

  test("orphan contacts"){
    val s = new DisconnectedSystemBuilder
    assert(orphanContactsRec(s) === List((".DisconnectedSystem", Set(s.c1, s.c2))))
  }

  class SuperSystem extends BaseTypedSystem("DisconnectedSuperSystem"){
    val pi1 = input[Int]("pi1")
    val po1 = output[Int]("po1")
    val subsystem1 = new DisconnectedSystemBuilder

    override protected def defineSystem(implicit sb: SystemBuilder) = {
      pi1 >> subsystem1.i1
      subsystem1.o1 >> po1
      sb.addSubsystem(subsystem1)
    }
  }

  test("orphan contacts in subsystem"){
    val s = new SuperSystem
    assert(subsystems(s).size === 2)
    assert(orphanContactsRec(s) ===
      List((".DisconnectedSuperSystem.DisconnectedSystem",
        Set(s.subsystem1.c1, s.subsystem1.c2))))
  }
}
