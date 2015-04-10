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
import ru.primetalk.synapse.core._

@RunWith(classOf[JUnitRunner])
class IncTest extends FunSuite {

  class SimpleSystemWithStateBuilder extends BaseTypedSystem{
    setSystemName("SimpleSystemWithState")
    val i1 = input[Int]("i1")
    val s1 = state[Int]("s1", 0)
    val o1 = output[Int]("o1")
    val o2 = output[Int]("o2")


    override protected def defineSystem(implicit sb: SystemBuilder) = {
      i1.inc(s1)
      i1.getState(s1).filter(_>10) >> o1

      val s2 = state[Int]("s2", 0)
      i1.addTo(s2)
      i1.getState(s2).filter(_>=45) >> o2
    }

  }

  val s = new SimpleSystemWithStateBuilder

  test("inc"){
    val tr = new RichDynamicSystem(s.toStaticSystem.toDynamicSystem).toTransducer(s.i1, s.o1)
    for(i<-0 until 10)
      assert(tr(i) === List())
    assert(tr(0) === List(11))
  }

  test("addTo"){
    val tr = new RichDynamicSystem(s.toStaticSystem.toDynamicSystem).toTransducer(s.i1, s.o2)
    assert((0 until 10).flatMap(tr) === List(45))
  }
}
