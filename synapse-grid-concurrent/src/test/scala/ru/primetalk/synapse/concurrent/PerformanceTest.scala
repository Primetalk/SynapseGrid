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
    val k = 400
    val n = 16

    val d = new HardWorker(k)
    val f = d.toStaticSystem.toParallelSimpleSignalProcessor.toTransducer(d.i1, d.o1)
    val g = d.toStaticSystem.toDynamicSystem.toTransducer(d.i1, d.o1)

    g(n) // preheat JIT
    val start = System.currentTimeMillis()
    val list = f(n)
    val delta = System.currentTimeMillis() - start
    assert(list.size === n)
    val start2 = System.currentTimeMillis()
    assert(list === g(n))
    val delta2 = System.currentTimeMillis() - start2
    println(s"k=$k, n=$n\npar: $delta\nseq: $delta2\nxRt: ${1.0*delta2/delta}\n")
    /*
    4000 / n=4
par: 3120
seq: 7836
xRt: 2.5115384615384615

par: 3164
seq: 7955
xRt: 2.514222503160556

after HTime.compare minor improvement:
par: 3083
seq: 7856
xRt: 2.5481673694453453

par: 3134
seq: 7914
xRt: 2.5252074026802807

Sort on addition:

par: 3081
seq: 7938
xRt: 2.5764362220058423

par: 3085
seq: 7895
xRt: 2.559157212317666

pre sorted running calculations
par: 3052
seq: 7908
xRt: 2.5910878112712976

par: 3068
seq: 7951
xRt: 2.5915906127770536

Busy computer

par: 3187
seq: 7977
xRt: 2.5029808597427046

par: 3176
seq: 7895
xRt: 2.485831234256927

par: 3191
seq: 8012
xRt: 2.5108116577875275

Moved to List

par: 3162
seq: 7748
xRt: 2.450347881087919

par: 3021
seq: 7839
xRt: 2.594836146971202

par: 3033
seq: 7721
xRt: 2.5456643587207384

stateless/stateful separation

par: 3209
seq: 7803
xRt: 2.4315986288563414

par: 3057
seq: 7776
xRt: 2.5436702649656526

     */

    /*
2000/n=8

par: 1721
seq: 3837
xRt: 2.2295177222545033

k=1000, n=16
par: 923
seq: 1910
xRt: 2.0693391115926327

k=1000, n=32
par: 1796
seq: 3817
xRt: 2.1252783964365256

k=500, n=16
par: 410
seq: 547
xRt: 1.3341463414634147

k=400, n=16
par: 332
seq: 322
xRt: 0.9698795180722891

k=400, n=16
par: 299
seq: 316
xRt: 1.0568561872909699

addSignals - non synchronized
k=400, n=16
par: 310
seq: 338
xRt: 1.0903225806451613

some minor changes; exception handling added
k=400, n=16
par: 305
seq: 328
xRt: 1.0754098360655737

k=400, n=16
par: 292
seq: 325
xRt: 1.1130136986301369

     */
  }
}
