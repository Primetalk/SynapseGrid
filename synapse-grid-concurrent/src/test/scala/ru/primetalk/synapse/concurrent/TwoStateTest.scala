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

import org.scalatest.FunSuite
import ru.primetalk.synapse.concurrent.ComputationState._
import ru.primetalk.synapse.core._
import scala.concurrent.ExecutionContext.Implicits.global

class TwoStateTest extends FunSuite{

  class TwoStates extends BaseTypedSystem{
    import sb._
    implicit val sb1 = sb
    setSystemName("Two states ordered")
    val i1 = input[Int]("i1")
    val integral1 = state[Int]("integral1", 0)
    val integral2 = state[Int]("integral2", 0)
    val m1 = contact[Int]("m1")
    val m2 = contact[Int]("m2")
    val integralSum = state[Int]("integralSum", 0)
    val o1 = output[Int]("o1")
    val inputs = i1.flatMap(0.until)

    def integrate(n:String):((Int, Int) => (Int, Int)) = {
      case (s:Int, i:Int) =>
        // in std.out n=1 and n=2 should appear in random order. However within each n the data should appear in order.
//        println(s"$n($s+$i)")
        (s+i, s+i)
    }
    val ccc1 = contact[Int]("ccc1")
    val ccc2 = contact[Int]("ccc2")
    i1.flatMap(0.until)>>ccc1
    ccc1.withState(integral1).stateMap(integrate("1"), "sm-i1") >> m1
    i1.flatMap(0.until)>>ccc2
    ccc2.withState(integral2).stateMap(integrate("2"), "sm-i2") >> m2

    m1.withState(integralSum).updateState()(_ + _)
    m2.withState(integralSum).updateState()(_ - _)

    i1.delay(3).getState(integralSum) >> o1
  }

  def performTest(): Unit = {
    val d = new TwoStates
    val f = d.toStaticSystem.toParallelSimpleSignalProcessor.toMapTransducer(d.i1, d.o1)
    val n = 50
    val m = f(n)
    //    println(list)
    assert(m === 0)
    //    val set = list.toSet
    //    assert(set.contains(0))
    //      "With proper order of signals plus and minus should come to integralSum in order.")
    //    assert(set === Set(0))
    val g = d.toStaticSystem.toDynamicSystem.toMapTransducer(d.i1, d.o1)
    assert(m === g(n))

  }
  test("Two states ordered"){
    for(i <- 0 until 10)
      performTest()
  }
}
