///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 *
 * Created: 24.09.13, zhizhelev
 */
package ru.primetalk.synapse.concurrent

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import ru.primetalk.synapse.concurrent.ComputationState._
import ru.primetalk.synapse.core.BaseTypedSystem

@RunWith(classOf[JUnitRunner])
class ComputationalGraphTest extends FunSuite{
  class SimpleFunctional extends BaseTypedSystem{
    import sb._
    val i1 = input[Int]("i1")
    val o1 = output[Int]("o1")
    i1.flatMap(t=>0 until t).map((i:Int) => i*i) >> o1
  }
  test("pure functional system"){
    val d = new SimpleFunctional
    val f = d.toStaticSystem.toParallelDynamicSystem.toTransducer(d.i1, d.o1)
    val n = 5
    val list = f(n)
    assert(list.size === n)
    val set = list.toSet
    assert(set.contains((n-1)*(n-1)))
    assert(set.contains(0))
    val g = d.toStaticSystem.toDynamicSystem.toTransducer(d.i1, d.o1)
    assert(list === g(n))
  }
  class SimpleStateful extends BaseTypedSystem{
    import sb._
    setSystemName("Ordered integration")
    val i1 = input[Int]("i1")
    val integral = state[Int]("integral", 0)
    val o1 = output[Int]("o1")
    i1.flatMap(0.until).
      withState(integral).stateMap{
        case (s, i) =>
          (s+i, s+i)
      } >> o1
  }
  test("stateful system"){

    val d = new SimpleStateful
    val f = d.toStaticSystem.toParallelDynamicSystem.toTransducer(d.i1, d.o1)
    val n = 5
    val list = f(n)
    assert(list.size === n)
    val set = list.toSet
    assert(set.contains(0))
    val g = d.toStaticSystem.toDynamicSystem.toTransducer(d.i1, d.o1)
    assert(list === g(n))
  }
}
