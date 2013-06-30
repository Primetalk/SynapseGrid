///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * ${PROJECT_NAME}
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 * (2-clause BSD license) See LICENSE
 *
 * Created: 28.06.13, zhizhelev
 */
package ru.primetalk.sinapse.core

/** The builder creates a state machine with `State` type of state.*/
trait AutomataBuilder[State] extends SystemBuilder {
	def initialState : State
	protected def stateName = "automatonState"
	protected val automatonState = state[State](stateName, initialState)

	private val zippedCache = scala.collection.mutable.Map[Contact[_], Contact[_]]()
	def zipped[T](c : Contact[T]) : Contact[(State, T)] = {
		zippedCache.getOrElseUpdate(c, c zipWithState (automatonState, "(State,_)")).asInstanceOf[Contact[(State, T)]]
	}
	private val zippedFilteredCache = scala.collection.mutable.Map[(Contact[_], State), Contact[_]]()
	def zippedFiltered[T](c : Contact[T], s:State) : Contact[T] = {
		zippedFilteredCache.getOrElseUpdate((c,s), zipped(c) flatMap (p ⇒ {
				if (p._1 == s)
					Seq(p._2)
				else
					Seq()
			},
				""+s+"?")).asInstanceOf[Contact[T]]
	}
	val saveToState = contact[State]("saveToState")
	lazy val onTransition = {
		val c = contact[(State, State)]("onTransition")
		saveToState -> c zipWithState automatonState
		c
	}
	saveToState.updateState(automatonState, "State!")((old, s) ⇒ s)
	implicit class StateContact[T](c : Contact[T]) {
		/** Constructs another contact that will get data when the automata is in the desired state. */
		def when(s : State) : Contact[T] =
			zippedFiltered(c,s)
		/**
		  * When signal appears on the contact the Automaton
		  *  moves to the desired state.
		  */
		def toState(s : State, name : String = ""):Contact[T]= {
//			c.updateState(automatonState, nextLabel(name, ""+s+"!"))((s1,t) => s) //.updateState(automatonState, ""+s+"!")((old, t)=>s)
			c -> saveToState map (t ⇒ s, nextLabel(name, ""+s+"!"))
			c
		}
		/** Switches to state and do some work along the way */
		def moveToState(s : State, name : String = "")(fun : T ⇒ Any) {
			c -> saveToState map (t ⇒ { fun(t); s }, nextLabel(name, ""+s+"!"))
		}
		def update(fun:(State,T)=>State, name : String = "") = {
			c.updateState(automatonState, nextLabel(name, "update automaton"))(fun)
			c
//			zipped(c) -> saveToState map (p=>fun(p._1, p._2), nextLabel(name, "update"))
		}
		/** Updates state unconditionally (ignores input data).*/
		def updateUnconditionally(fun:State=>State, name : String = ""){
			c.updateState(automatonState, nextLabel(name, "update uncoditionally"))((s, t) =>fun(s))
//			zipped(c) -> saveToState map (p=>fun(p._1), nextLabel(name, "update uncoditionally"))
		}

	}

	//Methods that deal with VAR currentConstructingState
	private var currentConstructingState :State = initialState
	def startBuildingState(s:State) = {
		currentConstructingState = s
		s
	}
	def inState(s : State)(body : ⇒ Unit) {
		currentConstructingState = s
		try {
			body
		} finally {
			currentConstructingState = initialState
		}
	}
//	implicit class StateConstructor(s : State) {
		def on[T](c:Contact[T]): Contact[T] = {new StateContact(c).when(currentConstructingState)}
//	}

//	def wait[T](c:Contact[T])(implicit s : State): Function1[()=>State, Unit] = (f:()=>State)=>{
//		(new StateContact(c).when(s) -> saveToState).map(m => f())//.toState(s)
//	}
//	}
}
