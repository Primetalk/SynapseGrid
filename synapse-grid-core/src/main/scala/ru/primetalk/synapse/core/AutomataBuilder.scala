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

/** The builder creates a state machine with `State` type of state.*/
trait AutomataBuilder[State] extends SystemBuilder {
	def initialState : State
	protected def stateName = "automatonState"
  /** All operations with automatonState should be done via DSL*/
	private val automatonState = state[State](stateName, initialState)


	private val zippedCache = scala.collection.mutable.Map[Contact[_], Contact[_]]()
	private def zipped[T](c : Contact[T]) : Contact[(State, T)] = {
		zippedCache.getOrElseUpdate(c, c zipWithState (automatonState, "(State,_)")).asInstanceOf[Contact[(State, T)]]
	}
	private val zippedFilteredCache = scala.collection.mutable.Map[(Contact[_], State), Contact[_]]()
  private def zippedFiltered[T](c : Contact[T], s:State) : Contact[T] = {
		zippedFilteredCache.getOrElseUpdate((c,s), zipped(c) flatMap (p ⇒ {
				if (p._1 == s)
					Seq(p._2)
				else
					Seq()
			},
				""+s+"?")).asInstanceOf[Contact[T]]
	}
  /** The only way to change automaton state is to save new state value into saveToState.*/
	val saveToState = contact[State]("saveToState")
  val onTransition = contact[(State, State)]("onTransition")
//  saveToState.updateState(automatonState, "State!")((old, s) ⇒ s)
  (saveToState -> onTransition).stateMap(automatonState){(oldState, newState) => (newState, (oldState, newState))}
  val onAutomatonStateChanged = contact[(State, State)]("onStateChanged")
  onTransition.labelNext("hasChanged?") -> onAutomatonStateChanged filter { case (oldState, newState) => newState != oldState }

  private val switchToStateCache = scala.collection.mutable.Map[ State, Contact[Any]]()
  def switchToState(s:State):Contact[Any] =
    switchToStateCache.getOrElseUpdate(s, {
      val c = auxContact[Any]
      c -> saveToState map (t ⇒ s, nextLabel("", ""+s+"!"))
      c
    })

  //  {
//		val c = contact[(State, State)]("onTransition")
//		saveToState -> c zipWithState automatonState
//		c
//	}
	implicit class StateContact[T](c : Contact[T]) {

    def zipWithAutomatonState:Contact[(State, T)] =
      zipped(c)
		/** Constructs another contact that will get data when the automata is in the desired state. */
		def when(s : State) : Contact[T] =
			zippedFiltered(c,s)

    // STATE TRANSITIONS
		/**
		  * When signal appears on the contact the Automaton
		  *  moves to the desired state.
		  */
		def toState(s : State, name : String = ""):Contact[T]= {
//			c.updateState(automatonState, nextLabel(name, ""+s+"!"))((s1,t) => s) //.updateState(automatonState, ""+s+"!")((old, t)=>s)
			c -> saveToState map (t ⇒ s, nextLabel(name, ""+s+"!"))
			c
		}
    def goto(s : State, name : String = ""):Contact[T]=
      toState(s,name)
		/** Switches to state and do some work along the way */
		def moveToState(s : State, name : String = "")(fun : T ⇒ Any) = {
			c -> saveToState map (t ⇒ { fun(t); s }, nextLabel(name, ""+s+"!"))
      c
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
  implicit class WhenState(val s:State){
    def on[T](c:Contact[T]): Contact[T] = new StateContact(c).when(s)
    def onEntered =
      onTransition.filter{ case (oldState, newState) => newState == s && oldState != s }
    /** a contact will */
    def ofExited =
      onTransition.filter{ case (oldState, newState) => oldState == s && newState != s }
  }
  def when(s:State) = new WhenState(s)
}
