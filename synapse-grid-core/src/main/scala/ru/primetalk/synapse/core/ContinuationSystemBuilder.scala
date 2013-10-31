///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 * (2-clause BSD license) See LICENSE
 *
 * Created: 28.06.13, zhizhelev
 */
package ru.primetalk.synapse.core

/**
 *
 * @author А.Жижелев (arseniy zhizhelev)
 */
trait ContinuationSystemBuilder extends SystemBuilder {
	type FCont[T1, T2] = T1 ⇒ Continuation[T1, T2]
	/**
	 * Returns some piece of code to run next time the data is
	 * passing via the link
	 */
	def next[T1, T2](body: FCont[T1, T2]): Continuation[T1, T2] =
		ContinuationCalcWithResult(body, Seq[T2]())

	/**
	 * returns both the piece of code to run next time and the data to
	 * return this time.
	 */
	def nextAndPass[T1, T2](body: FCont[T1, T2], intermediateResult: Seq[T2]): Continuation[T1, T2] =
		ContinuationCalcWithResult(body, intermediateResult)
	/**
	 * returns both the piece of code to run next time and the data to
	 * return this time.
	 *
	 * The same as nextAndPass but with different order of arguments.
	 */
	def passAndNext[T1, T2](intermediateResult: Seq[T2])(body: FCont[T1, T2]): Continuation[T1, T2] =
		ContinuationCalcWithResult(body, intermediateResult)
	/**
	 * Go into the final state
	 */
	def done[T1, T2](result: Seq[T2]): Continuation[T1, T2] =
		Done(result)
	/**
	 * Done with empty results.
	 */
	def doneFinal[T1, T2]: Continuation[T1, T2] =
		Done(Seq[T2]())

	implicit class ImplContLinkBuilder[T1, T2](c : (Contact[T1], Contact[T2])){
		def expectingFlatMap(function : Continuation[T1, T2], name : String = "") = {
			val stateHolder = state[Continuation[T1, T2]](name, function)
			new ImplStateLinkBuilder(c).stateFlatMap[Continuation[T1, T2]](stateHolder, nextLabel(name, "expect")){
				(s0 : Continuation[T1, T2], d : T1) ⇒
					s0 match {
						case Done(result) ⇒ (s0, result)
						case ContinuationCalcWithResult(f, result) ⇒
							val cont = f(d)
							(cont, cont.result)
						//						case ContinuationCalcNoResult(f) => (f(d), Seq())
					}
      }

		}
		def expectingNext(f : T1 ⇒ Continuation[T1, T2],
				name : String = "") =
					expectingFlatMap(ContinuationCalcWithResult(f, Seq[T2]()), nextLabel(name, "expect"))

	}
}
/**
 * The continuation usually has a function that will return new continuation.
 * The current continuation has result that is returned immediately when
 * the continuation is created. Later this result can be considered as a hint
 * of what was returned.
 */
sealed trait Continuation[T1, T2] {
  val result : Seq[T2]
}
/**
 * a continuation with calculations. Returns some result before
 * entering expectation state
 */
case class ContinuationCalcWithResult[T1, T2](f : T1 ⇒ Continuation[T1, T2], result : Seq[T2]) extends Continuation[T1, T2]

case class Done[T1, T2](result : Seq[T2]) extends Continuation[T1, T2]
