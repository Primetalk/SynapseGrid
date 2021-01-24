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

import org.junit.Test
import ru.primetalk.synapse.core.syntax._
import ru.primetalk.synapse.core.syntax.given

import ru.primetalk.synapse.concurrent.ComputationState._
import scala.concurrent.ExecutionContext.Implicits.global

class ComputationalGraphTest {
  class SimpleFunctional extends BaseTypedSystem{
    implicit val sb1: SystemBuilder = sb
    val i1 = input[Int]("i1")
    val o1 = output[Int]("o1")
    i1.flatMap(t=>0 until t).map((i:Int) => i*i) >> o1
  }
  @Test def `pure functional system`(): Unit = {
    val d = new SimpleFunctional
    val f = d.toStaticSystem.toParallelDynamicSystem.toTransducer(d.i1, d.o1)
    val n = 5
    val list = f(n)
    assert(list.iterator.size == n)
    val set = list.iterator.toSet
    assert(set.contains((n-1)*(n-1)))
    assert(set.contains(0))
    val g = d.toStaticSystem.toDynamicSystem.toTransducer(d.i1, d.o1)
    assert(list == g(n))
  }
  class SimpleStateful extends BaseTypedSystem{
    implicit val sb1: SystemBuilder = sb
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
  @Test def `stateful system`(): Unit = {

    val d = new SimpleStateful
    val f = d.toStaticSystem.toParallelDynamicSystem.toTransducer(d.i1, d.o1)
    val n = 5
    val list = f(n)
    assert(list.iterator.size == n)
    val set = list.iterator.toSet
    assert(set.contains(0))
    val g = d.toStaticSystem.toDynamicSystem.toTransducer(d.i1, d.o1)
    assert(list == g(n))
  }
}
