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
import ru.primetalk.synapse.core.BaseTypedSystem

import ComputationState._

class PerformanceTest extends FunSuite{

  class HardWorker(innerLoop:Int) extends BaseTypedSystem{
    import sb._
    setSystemName("HardWorker")
    val i1 = input[Int]("i1")
    val m1 = contact[Int]("m1")
    val o1 = output[Int]("o1")
    val inputs = i1.flatMap(0.until)

    /** works hard. ignores input */
    def longWay(i:Int) = {
      (0 until innerLoop).toList.map(j => (0 until innerLoop) .map(_ => 10*math.cos(j)).sum).sum.toInt
    }
    inputs.map(longWay) >> o1

  }
  test("Two states ordered"){
    import scala.concurrent.ExecutionContext.Implicits.global
    val d = new HardWorker(4000)
    val f = d.toStaticSystem.toParallelSimpleSignalProcessor.toTransducer(d.i1, d.o1)
    val n = 4
    val start = System.currentTimeMillis()
    val list = f(n)
    val delta = System.currentTimeMillis() - start
    assert(list.size === n)
    val g = d.toStaticSystem.toDynamicSystem.toTransducer(d.i1, d.o1)
    val start2 = System.currentTimeMillis()
    assert(list === g(n))
    val delta2 = System.currentTimeMillis() - start2
    println(s"par: $delta\nseq: $delta2\nxRt: ${1.0*delta2/delta}")
  }
}
