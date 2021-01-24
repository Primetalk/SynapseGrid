/**
 *
 */
package ru.primetalk.synapse.core.managed

import org.junit.Test
import ru.primetalk.synapse.core.syntax._
import ru.primetalk.synapse.core.syntax.given

/**
 * @author nehaev
 *
 */
class ManagedStateTest {
	
	object AInput extends Contact[Int]("AInput")
	object BInput extends Contact[Int]("BInput")
	object CInput extends Contact[Int]("CInput")
	object ABOutput extends Contact[Int]("ABOutput")
	object ABCOutput extends Contact[Int]("ABCOutput")
	
	/**
	 * Calculation of (a+b)*c expression using managed states.
	 */
	class ABCBuilder extends BaseTypedSystem {
		sb.inputs(AInput, BInput, CInput)
		sb.outputs(ABOutput, ABCOutput)


		override protected def defineSystem(implicit sb: SystemBuilder): Unit = {

			val a = managedState[Int]("a")
			val b = managedState[Int]("b")
			val c = managedState[Int]("c")
			val `a+b` = managedState[Int]("a+b")
			val `(a+b)*c` = managedState[Int]("(a+b)*c")

			AInput >>: a
			BInput >>: b
			CInput >>: c

			val recalcAB: Contact[Int] = contact[Int]("recalcAB")
			a >> recalcAB
			b >> recalcAB
			recalcAB.zipWithManagedState(a).zipWithManagedState(b).map { abp =>
				val (bv, (av, _)) = abp
				av + bv
			} saveToManagedState `a+b`

			val recalcABC = contact[Int]("recalcABC")

			c >> recalcABC

			`a+b` >> recalcABC

			recalcABC.zipWithManagedState(`a+b`).zipWithManagedState(c).map { scp =>
				val (cv, (sv, _)) = scp
				sv * cv
			} saveToManagedState `(a+b)*c`

			`a+b` >> ABOutput

			`(a+b)*c` >> ABCOutput
		}
	}
	
	/**
	 * Calculation of (a+b)*c expression using managed states.
	 */
	class ABCShortBuilder extends BaseTypedSystem {

//		implicit val sb1 = sb
		sb.inputs(AInput, BInput, CInput)
		sb.outputs(ABOutput, ABCOutput)
		override protected def defineSystem(implicit sb: SystemBuilder): Unit = {
			val a = managedState[Int]("a")
			AInput >>: a
			val b = managedState[Int]("b")
			BInput >>: b
			val c = managedState[Int]("c")
			CInput >>: c

			val `a+b` = managedState[Int]("a+b")

			`a+b` >> ABOutput

			val `(a+b)*c` = managedState[Int]("(a+b)*c")

			`(a+b)*c` >> ABCOutput

			`a+b`.dependsOn(a, b)(_ + _)

			`(a+b)*c`.dependsOn(`a+b`, c) (_ * _)

		}
	}
	

	val abcSystem: StaticSystem = new ABCBuilder().toStaticSystem
	val abcShortSystem: StaticSystem = new ABCShortBuilder().toStaticSystem
	
	
	
	@Test def `a+b`(): Unit = {
		val abcCalc = abcSystem.toDynamicSystem
		val aSig = AInput.createSignal(4)
		val outSigs1 = abcCalc.receive(aSig)
		assert(outSigs1.iterator.isEmpty, s"outSigs1 === $outSigs1")
		
		val bSig = BInput.createSignal(3)
		val outSigs2 = abcCalc.receive(bSig)
		assert(outSigs2.iterator.size == 1, s"outSigs2 === $outSigs2")
		assert(ABOutput.filterFunction(outSigs2).toList.head.data == 7)
	}


	@Test def `a+b*c`(): Unit = {
		val abcCalc = abcSystem.toDynamicSystem
		val aSig = AInput.createSignal(4)
		abcCalc.receive(aSig)
		
		val bSig = BInput.createSignal(3)
		abcCalc.receive(bSig)
		
		val cSig = CInput.createSignal(5)
		val outSigs3 = abcCalc.receive(cSig)
		assert(outSigs3.iterator.size == 1)
		assert(ABCOutput.filterFunction(outSigs3).toList.head.data == 35)
	}

	@Test def `a+b*c recals`(): Unit = {
		val abcCalc = abcSystem.toDynamicSystem
		val aSig = AInput.createSignal(4)
		abcCalc.receive(aSig)
		
		val bSig = BInput.createSignal(3)
		abcCalc.receive(bSig)
		
		val cSig = CInput.createSignal(5)
		abcCalc.receive(cSig)
		
		
		val bSig2 = BInput.createSignal(6)
		val outSigs4 = abcCalc.receive(bSig2)
		assert(outSigs4.iterator.size == 2, s"outSigs4 === $outSigs4")
		assert(ABOutput.filterFunction(outSigs4).toList.head.data == 10)
		assert(ABCOutput.filterFunction(outSigs4).toList.head.data == 50)
		
		val cSig2 = CInput.createSignal(2)
		val outSigs5 = abcCalc.receive(cSig2)
		assert(outSigs5.iterator.size == 1, s"outSigs5 === $outSigs5")
		assert(ABCOutput.filterFunction(outSigs5).toList.head.data == 20)
	}

	@Test def `short a+b`(): Unit = {
		val abcCalc = abcShortSystem.toDynamicSystem
		val aSig = AInput.createSignal(4)
		val outSigs1 = abcCalc.receive(aSig)
		assert(outSigs1.iterator.size == 0)
		
		val bSig = BInput.createSignal(3)
		val outSigs2 = abcCalc.receive(bSig)
		assert(outSigs2.iterator.size == 1)
		assert(ABOutput.filterFunction(outSigs2).toList.head.data == 7)
	}

	@Test def `short a+b*c`(): Unit = {
		val abcCalc = abcShortSystem.toDynamicSystem
		val aSig = AInput.createSignal(4)
		abcCalc.receive(aSig)
		
		val bSig = BInput.createSignal(3)
		abcCalc.receive(bSig)
		
		val cSig = CInput.createSignal(5)
		val outSigs3 = abcCalc.receive(cSig)
		assert(outSigs3.iterator.size == 1)
		assert(ABCOutput.filterFunction(outSigs3).toList.head.data == 35)
	}

	@Test def `short a+b *c recalc`(): Unit = {
		val abcCalc = abcShortSystem.toDynamicSystem
		val aSig = AInput.createSignal(4)
		abcCalc.receive(aSig)
		
		val bSig = BInput.createSignal(3)
		abcCalc.receive(bSig)
		
		val cSig = CInput.createSignal(5)
		abcCalc.receive(cSig)
		
		
		val bSig2 = BInput.createSignal(6)
		val outSigs4 = abcCalc.receive(bSig2)
		assert(outSigs4.iterator.size == 2)
		assert(ABOutput.filterFunction(outSigs4).toList.head.data == 10)
		assert(ABCOutput.filterFunction(outSigs4).toList.head.data == 50)
		
		val cSig2 = CInput.createSignal(2)
		val outSigs5 = abcCalc.receive(cSig2)
		assert(outSigs5.iterator.size == 1)
		assert(ABCOutput.filterFunction(outSigs5).toList.head.data == 20)
	}
	@Test def `no orphans`(): Unit = {
    assert(orphanContactsRec(abcShortSystem)==List())
  }
	
}