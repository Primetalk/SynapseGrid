///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 * (2-clause BSD license) See LICENSE
 *
 * Created: 30.06.13, zhizhelev
 */
package ru.primetalk.synapse.examples

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SimpleExamplesTest extends FunSuite{
  import ru.primetalk.synapse.examples.SimpleExamples._
  test("example 1"){
    assert(e1("hello") === 5)
  }
  test("example 2"){
    assert(e2("hello bye") === List("hello", "bye"))
  }
  test("example 3"){
    assert(e3("hello") === 1)
    assert(e3("hello") === 2)
    assert(e3("hello") === 3)
  }
//  test("example 2")
}
