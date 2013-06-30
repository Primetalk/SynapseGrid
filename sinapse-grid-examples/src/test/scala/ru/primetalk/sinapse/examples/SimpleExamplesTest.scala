///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SinapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 * (2-clause BSD license) See LICENSE
 *
 * Created: 30.06.13, zhizhelev
 */
package ru.primetalk.sinapse.examples

import org.scalatest.FunSuite

class SimpleExamplesTest extends FunSuite{
  import SimpleExamples._
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
}
