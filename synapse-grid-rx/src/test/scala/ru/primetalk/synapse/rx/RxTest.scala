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
 * Created: 06.12.13, zhizhelev
 */
package ru.primetalk.synapse.rx

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import ru.primetalk.synapse.core._

@RunWith(classOf[JUnitRunner])
class RxTest extends FunSuite {
  class LengthCalculator extends BaseTypedSystem{
    import sb._
    implicit val isb = sb
    val textInput = input[String]("textInput")
    val lengthOutput = output[Int]("lengthOutput")
    (textInput->lengthOutput).map(_.length)

    val debugObservable = textInput.map("debug:"+_).toObservable
  }
  test("RxJava DSL test"){
    val s = new LengthCalculator
    val rxs = new DynamicSystemRx(s.toStaticSystem.toDynamicSystem)
    var len = 0
    var debugHello = ""
    rxs.rxOutput(s.lengthOutput).subscribe((l:Int) => len = l)
    s.debugObservable.subscribe(s => debugHello = s)

    rxs.rxInput(s.textInput).onNext("Hello")
    assert(len === 5)
    assert(debugHello === "debug:Hello")
  }
}
