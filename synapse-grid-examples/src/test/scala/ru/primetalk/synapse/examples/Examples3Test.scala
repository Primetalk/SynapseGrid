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

import org.scalatest.FunSuite

import ru.primetalk.synapse.core._
import ru.primetalk.synapse.examples.Examples3.SuperSystemBuilder

class Examples3Test extends FunSuite {
  test("Dump"){
    new SuperSystemBuilder().toDotAtLevel(2).saveTo("SuperSystem.dot")
  }

}
