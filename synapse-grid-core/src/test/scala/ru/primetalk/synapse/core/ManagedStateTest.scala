/**
 *
 */
package ru.primetalk.synapse.core

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import Contacts._

/**
 * @author nehaev
 *
 */
@RunWith(classOf[JUnitRunner])
class ManagedStateTest extends FunSuite {
	
	object AInput extends Contact[Int]("AInput")
	object BInput extends Contact[Int]("BInput")
	object CInput extends Contact[Int]("CInput")
	object ABOutput extends Contact[Int]("ABOutput")
	object ABCOutput extends Contact[Int]("ABCOutput")
	
	/**
	 * Calculation of (a+b)*c expression using managed states.
	 */
	class ABCBuilder extends SystemBuilderWithManagedStates {
		
		inputs(AInput, BInput, CInput)
		outputs(ABOutput, ABCOutput)
		
		val a = managedState[Int]("a")
		val b = managedState[Int]("b")
		val c = managedState[Int]("c")
		val `a+b` = managedState[Int]("a+b")
		val `(a+b)*c` = managedState[Int]("(a+b)*c")
		
		AInput.saveToManagedState(a)
		BInput.saveToManagedState(b)
		CInput.saveToManagedState(c)

		val recalcAB = contact[Int]("recalcAB")
		a.onUpdated >> recalcAB
		b.onUpdated >> recalcAB
		recalcAB.zipWithManagedState(a).zipWithManagedState(b).map { abp ⇒
			val (bv, (av, _)) = abp
			av + bv
		} saveToManagedState (`a+b`)
		
		val recalcABC = contact[Int]("recalcABC")
		c.onUpdated >> recalcABC
		`a+b`.onUpdated >> recalcABC
		recalcABC.zipWithManagedState(`a+b`).zipWithManagedState(c).map { scp ⇒
			val (cv, (sv, _)) = scp
			sv * cv
		} saveToManagedState (`(a+b)*c`)
		
		`a+b`.onUpdated >> ABOutput
		`(a+b)*c`.onUpdated >> ABCOutput
	}
	
	/**
	 * Calculation of (a+b)*c expression using managed states.
	 */
	class ABCShortBuilder extends SystemBuilderWithManagedStates {
		
		inputs(AInput, BInput, CInput)
		outputs(ABOutput, ABCOutput)
		
		val a = managedState[Int]("a").fillFrom(AInput)
		val b = managedState[Int]("b").fillFrom(BInput)
		val c = managedState[Int]("c").fillFrom(CInput)
		
		val `a+b` = managedState[Int]("a+b").passTo(ABOutput)
		val `(a+b)*c` = managedState[Int]("(a+b)*c").passTo(ABCOutput)
		

		`a+b`.dependsOn(a, b) { (av, bv) ⇒
			av + bv
		}

		`(a+b)*c`.dependsOn(`a+b`, c) { (sv, cv) ⇒
			sv * cv
		}
	}
	

	val abcSystem = new ABCBuilder().toStaticSystem
	val abcShortSystem = new ABCShortBuilder().toStaticSystem
	
	
	
	test("a+b") {
		val abcCalc = abcSystem.toDynamicSystem
		val aSig = AInput.createSignal(4)
		val outSigs1 = abcCalc.receive(aSig)
		assert(outSigs1.size === 0)
		
		val bSig = BInput.createSignal(3)
		val outSigs2 = abcCalc.receive(bSig)
		assert(outSigs2.size === 1)
		assert(ABOutput.filterFunction(outSigs2).head.data === 7)
	}
	
	
	test("a+b *c") {
		val abcCalc = abcSystem.toDynamicSystem
		val aSig = AInput.createSignal(4)
		val outSigs1 = abcCalc.receive(aSig)
		
		val bSig = BInput.createSignal(3)
		val outSigs2 = abcCalc.receive(bSig)
		
		val cSig = CInput.createSignal(5)
		val outSigs3 = abcCalc.receive(cSig)
		assert(outSigs3.size === 1)
		assert(ABCOutput.filterFunction(outSigs3).head.data === 35)
	}
	
	test("a+b *c recalc") {
		val abcCalc = abcSystem.toDynamicSystem
		val aSig = AInput.createSignal(4)
		val outSigs1 = abcCalc.receive(aSig)
		
		val bSig = BInput.createSignal(3)
		val outSigs2 = abcCalc.receive(bSig)
		
		val cSig = CInput.createSignal(5)
		val outSigs3 = abcCalc.receive(cSig)
		
		
		val bSig2 = BInput.createSignal(6)
		val outSigs4 = abcCalc.receive(bSig2)
		assert(outSigs4.size === 2)
		assert(ABOutput.filterFunction(outSigs4).head.data === 10)
		assert(ABCOutput.filterFunction(outSigs4).head.data === 50)
		
		val cSig2 = CInput.createSignal(2)
		val outSigs5 = abcCalc.receive(cSig2)
		assert(outSigs5.size === 1)
		assert(ABCOutput.filterFunction(outSigs5).head.data === 20)
	}
	
	
	test("short a+b") {
		val abcCalc = abcShortSystem.toDynamicSystem
		val aSig = AInput.createSignal(4)
		val outSigs1 = abcCalc.receive(aSig)
		assert(outSigs1.size === 0)
		
		val bSig = BInput.createSignal(3)
		val outSigs2 = abcCalc.receive(bSig)
		assert(outSigs2.size === 1)
		assert(ABOutput.filterFunction(outSigs2).head.data === 7)
	}
	
	
	test("short a+b *c") {
		val abcCalc = abcShortSystem.toDynamicSystem
		val aSig = AInput.createSignal(4)
		val outSigs1 = abcCalc.receive(aSig)
		
		val bSig = BInput.createSignal(3)
		val outSigs2 = abcCalc.receive(bSig)
		
		val cSig = CInput.createSignal(5)
		val outSigs3 = abcCalc.receive(cSig)
		assert(outSigs3.size === 1)
		assert(ABCOutput.filterFunction(outSigs3).head.data === 35)
	}
	
	test("short a+b *c recalc") {
		val abcCalc = abcShortSystem.toDynamicSystem
		val aSig = AInput.createSignal(4)
		val outSigs1 = abcCalc.receive(aSig)
		
		val bSig = BInput.createSignal(3)
		val outSigs2 = abcCalc.receive(bSig)
		
		val cSig = CInput.createSignal(5)
		val outSigs3 = abcCalc.receive(cSig)
		
		
		val bSig2 = BInput.createSignal(6)
		val outSigs4 = abcCalc.receive(bSig2)
		assert(outSigs4.size === 2)
		assert(ABOutput.filterFunction(outSigs4).head.data === 10)
		assert(ABCOutput.filterFunction(outSigs4).head.data === 50)
		
		val cSig2 = CInput.createSignal(2)
		val outSigs5 = abcCalc.receive(cSig2)
		assert(outSigs5.size === 1)
		assert(ABCOutput.filterFunction(outSigs5).head.data === 20)
	}
	
}