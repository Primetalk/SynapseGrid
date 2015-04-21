///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////

/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 * <p/>
 * Created: 17.07.13, zhizhelev
 */
package ru.primetalk.synapse.examples

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import ru.primetalk.synapse.core._
import ru.primetalk.synapse.examples.Examples3.SuperSystemBuilder

@RunWith(classOf[JUnitRunner])
class Examples3Test extends FunSuite {
  test("Dump"){
    val ss = new SuperSystemBuilder()
    assert(orphanContactsRec(ss) === List())
    ss.toDot(2).saveTo("SuperSystem.dot")
  }

}
