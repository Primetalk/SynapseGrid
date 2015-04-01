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
import ru.primetalk.synapse.core._
import ru.primetalk.synapse.concurrent.ComputationState._

@RunWith(classOf[JUnitRunner])
class SubsystemTest extends FunSuite{

  class TwoStatesInnerSubsystem(name:String) extends BaseTypedSystem{
    import sb._
    implicit val sb1 = sb
    setSystemName("TwoStatesInnerSubsystem")
    val i1 = input[Int]("i1")
    val integral1 = state[Int]("integral1", 0)
    val integral2 = state[Int]("integral2", 0)
    val m1 = contact[Int]("m1")
    val m2 = contact[Int]("m2")
    val integralSum = state[Int]("integralSum", 0)
    val o1 = output[Int]("o1")
//    val inputs = i1.flatMap(0.until)

    def integrate(n:String):((Int, Int) => (Int, Int)) = {
      case (s:Int, i:Int) =>
        // in std.out n=1 and n=2 should appear in random order. However within each n the data should appear in order.
//        println(s"$n($s+$i)")
        (s+i, s+i)
    }
    i1.flatMap(0.until).withState(integral1).stateMap(integrate("1")) >> m1
    i1.flatMap(0.until).withState(integral2).stateMap(integrate("2")) >> m2

    m1.withState(integralSum).updateState()(_ + _)
    m2.withState(integralSum).updateState()(_ - _)

    i1.delay(3).getState(integralSum) >> o1
  }

  class OuterSystem extends BaseTypedSystem {
    import sb._
    implicit val sb1 = sb
    setSystemName("OuterSystem")
    val outerInput1 = input[Int]("outerInput1")
    val outerOutput1 = output[Int]("outerOutput1")
    private val inner = new TwoStatesInnerSubsystem("inner")
    addSubsystem(inner)
    outerInput1 >> inner.i1
//    inner.o1.foreach(println)
    inner.o1 >> outerOutput1

  }
  def performTest() {
    import scala.concurrent.ExecutionContext.Implicits.global
    val d = new OuterSystem
    val f = d.toStaticSystem.toParallelSimpleSignalProcessor.toMapTransducer(d.outerInput1, d.outerOutput1)
    val n = 50
    val m = f(n)
    val g = d.toStaticSystem.toDynamicSystem.toMapTransducer(d.outerInput1, d.outerOutput1)
    assert(m === g(n))

  }
  test("Two states ordered"){
    for(i <- 0 until 10)
      performTest()

  }
}
