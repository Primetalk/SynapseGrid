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
 * Created: 21.09.13, zhizhelev
 */
package ru.primetalk.synapse.concurrent

import ru.primetalk.synapse.core._
import scala.concurrent.{Await, Future, ExecutionContext}
import scala.language.existentials
import ru.primetalk.synapse.core.Signal
import ru.primetalk.synapse.core.RuntimeComponentStateFlatMap
import ru.primetalk.synapse.core.DynamicSystem
import ru.primetalk.synapse.core.RuntimeComponentFlatMap
import ru.primetalk.synapse.core.RuntimeComponentMultiState
import ru.primetalk.synapse.core.RuntimeSystem
import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.collection.mutable


/** The prerequisites for the UnitOfComputation
  * Requires that the states have got their values for AtTime moment.
  * The computation unit will softly block over this state. */
case class StateRequirement(stateHandles: List[Contact[_]], time:HTime) //extends Requirement


/**
 * The unit of computation may have constraints:
 * - list of states @time
 *
 * Also it can have properties
 * - update state@time
 *
 * Units are interconnected with dependencies:
 * - incoming units of computation
 * - dependent units of computation
 *
 * interconnection is given outside
 */
case class UnitOfComputation(signalAtTime: AtTime[Signal[_]], // the signal that triggered the computation.
                             rc: RuntimeComponent){
  val stateRequirement = rc match {
    case RuntimeComponentFlatMap(name, _, _, f) =>
      StateRequirement(List(),signalAtTime.time)
    case RuntimeComponentStateFlatMap(name, _, _, sh, f) =>
      StateRequirement(List(sh), signalAtTime.time)
    case RuntimeComponentMultiState(name, stateHandles, f) =>
      StateRequirement(stateHandles, signalAtTime.time)
  }
}
case class ComputationCompleted(
                                 computation:UnitOfComputation,
                                 newSignals:List[AtTime[Signal[_]]],
                                 statesToRelease:List[AtTime[Contact[_]]])

case class RunningUnitOfComputation(u: UnitOfComputation, future: Future[ComputationCompleted])



/** A shared state that is used by planner between threads.
  *
  * Computational units and time moments are connected
  * with Happens-before primitive
  *
  * If a function has side effects it should declare the dependency on
  * a state handle "GlobalState". Then the planner will avoid running
  * the function in parallel with other side-effect functions. All other
  * simple functions are considered as side-effect free.
  *
  * The signals are promoted to have `time moment` in order to ensure
  * trellis properties.
  *
  */
class ComputationState(rs: RuntimeSystem,
                       state0:Map[Contact[_], _])(implicit val executionContext: ExecutionContext) {

  private var runningCalculationsSorted = List[RunningUnitOfComputation]()
  private var computationQueueSorted = List[UnitOfComputation]()

  /** Absolute past. No signal can appear before this time. */
  private var pastTimeBoundary = 0
  private var outputs = List[AtTime[Signal[_]]]()

  private var failure: Option[Throwable] = None

  private
  def addComputations(units:List[UnitOfComputation]){
    this.synchronized{
      computationQueueSorted = units ::: computationQueueSorted
      computationQueueSorted = computationQueueSorted.sortBy(_.signalAtTime.time)
    }
  }

  private
  def addRunningComputation(r:RunningUnitOfComputation){
    runningCalculationsSorted = r +: runningCalculationsSorted
    runningCalculationsSorted = runningCalculationsSorted.sortBy(_.u.signalAtTime.time)
  }

  private[concurrent] val variables:Map[Contact[_], SafeVariable[_]] =
    state0.map{ case (sh, initialValue) => (sh, new SafeVariable(initialValue))}.toMap


  /** Changes context to the given value*/
  def resetContext(context:Context){
    this.synchronized{
      for{sh <- state0.keys}{
        val v = variables(sh).asInstanceOf[SafeVariable[Any]]
        v.update(old => context(sh))
      }
    }
  }
  def getContext:Context =
    this.synchronized{
      state0.keys.
        map(sh =>
          (sh,
            variables(sh).asInstanceOf[SafeVariable[Any]].get)).
        toMap
    }

  /** Add a single signal.
    *
    * Starts immediate computations if requirements are met.
    * Adds them to running. Those computations that cannot be started immediately
    * will be put to the computation plan. */
  def addSignal(signalAtTime: AtTime[Signal[_]]) {
    val AtTime(time, Signal(contact, _)) = signalAtTime
    if (rs.stopContacts.contains(contact))
      this.synchronized {
        outputs = signalAtTime :: outputs
      }
    else {
      val components = rs.signalProcessors(contact)
      val computationUnits = components.zipWithIndex.map{ case (c, i) =>
        val signalForComponent = //AtTime.placeAfter(signalAtTime.time,  signalAtTime.value)//
         signalAtTime.copy(time = time.next(i))
        UnitOfComputation(signalForComponent, c)
      }
      val (stateful, stateless) = computationUnits.partition(_.rc.isStateful)
      stateless.foreach(start)
      addComputations(stateful)
    }
  }

  def addSignals(signalsAtTime:List[ AtTime[Signal[_]]]){
    signalsAtTime.foreach(addSignal)
  }
  private
  def start(t: UnitOfComputation) {
    this.synchronized{
      if(failure.isDefined)
        return
      import t._
      t.stateRequirement.stateHandles.foreach{stateHandle=>
        variables(stateHandle).lock.acquire()
      }
      val signal = signalAtTime.value
      val time = signalAtTime.time

      val future = rc match {
        case RuntimeComponentFlatMap(name, _, _, f) =>
          Future {
            val signals = f(signal)
            ComputationCompleted(t, AtTime.placeAfter(time, signals), List())
          }
        case RuntimeComponentStateFlatMap(name, _, _, sh, f) =>
          val v = variables(sh).asInstanceOf[SafeVariable[Any]]
          val f2 = f.asInstanceOf[((Any, Signal[_]) => (Any, List[Signal[_]]))] //(state, signal)=>(state, signals)
          Future {
            val signals = v.update2 {
              oldValue => f2(oldValue, signal)
            }
            ComputationCompleted(t, AtTime.placeAfter(time, signals), List(AtTime(signalAtTime.time.next(-1), sh)))
          }
        case RuntimeComponentMultiState(name, stateHandles, f) =>
          val f2 = f.asInstanceOf[((Context, Signal[_]) => (Context, List[Signal[_]]))] //(state, signal)=>(state, signals)
          Future {
            val context = stateHandles.map(sh =>
              (sh.asInstanceOf[Contact[Any]], variables(sh).get)).toMap.asInstanceOf[Context]//[Contact[_], _]
            val (newContext, signals) = f2(context, signal)
            for((sh, v) <- newContext.asInstanceOf[Context])
              variables(sh).asInstanceOf[SafeVariable[Any]].update(t => v)
            ComputationCompleted(t, AtTime.placeAfter(time, signals), AtTime.placeAfter(time, stateHandles))
          }
      }
      future.onSuccess {
        case cc => computationCompleted(cc)
      }
      future.onFailure{
        case exception => computationFailed(t, exception)
      }
      addRunningComputation(RunningUnitOfComputation(t, future) )
    }
  }

  private
  def computationCompleted(computationCompleted:ComputationCompleted) {
    import computationCompleted._
    addSignals(newSignals)// can be non blocking.
    this.synchronized{ // TODO O(n)->O(1) (map)
      runningCalculationsSorted = runningCalculationsSorted.filterNot(_.u eq computation)
      statesToRelease.foreach(stateHandleAtTime=>variables(stateHandleAtTime.value).lock.release())
    }
    if(!statesToRelease.isEmpty)
      fastCheckPlan()// check plan only if there were states.
  }
  private
  def computationFailed(u:UnitOfComputation, e:Throwable){
    this.synchronized{
      val rt = new RuntimeException(s"Exception during $u processing.", e)
      rt.fillInStackTrace()
      if(failure.isEmpty)
        failure = Some(rt)
    }
  }
  /**
    Runs computations for which requirements are met.

    During execution we select earlier signals that can be
    processed without violating computation constraints.

    The planner plans simple sequences that can be calculated in a row.

    The planning is performed whenever an executor has
    nothing to do independently. Before planning next steps the independently
    generated trellis is merged into the trunk one.

    Every time when planning is performed, signals that can be processed
    independently are removed from trellis and corresponding tasks are created.

    For links that depend on a state the state is moved to a soft lock list
    with the time moment.
    The state processing is not started until all previous time moments
    have been handled.

   */
  private
  def checkPlan() {
    this.synchronized {
      val times = Int.MaxValue ::
        runningCalculationsSorted.headOption.toList.map(_.u.signalAtTime.time.trellisTime) :::
          computationQueueSorted.headOption.toList.map(_.signalAtTime.time.trellisTime)
      pastTimeBoundary = times.min
      fastCheckPlan()
    }
  }

  /** Do not sort computations and update past time boundary. Just tries to find easy jobs.*/
  private
  def fastCheckPlan() {
    this.synchronized {
      val delayed = mutable.ListBuffer[UnitOfComputation]()
      var alreadyRequiredStates = runningCalculationsSorted.flatMap(_.u.stateRequirement.stateHandles).toSet
      for{unit <- computationQueueSorted}{
        import unit._
        import stateRequirement._
        if(stateHandles.isEmpty)
          start(unit)
        else{
          if(time.trellisTime != pastTimeBoundary)
            delayed += unit
          else{
            if (
              stateHandles.forall(sh =>
                !alreadyRequiredStates.contains(sh) &&
                  variables(sh).lock.available)
            )
              start(unit)
            else {
              alreadyRequiredStates = alreadyRequiredStates ++ stateHandles
              delayed += unit
            }
          }
        }
      }
      computationQueueSorted = delayed.toList
    }
  }


  private
  def waitUntilCompleted(atMostPerAwaitableFuture:Duration = Duration.Inf) {
    @tailrec
    def waitUntilCompleted0():Unit = {
      val (failureIsDefined, runningHeadOpt, computationQueueIsEmpty) = this.synchronized{
        (failure.isDefined, runningCalculationsSorted.headOption, computationQueueSorted.isEmpty)
      }
      if(failureIsDefined || (runningHeadOpt.isEmpty && computationQueueIsEmpty))
        ()
      else {
        if(!runningHeadOpt.isEmpty)
          Await.result(runningHeadOpt.get.future, atMostPerAwaitableFuture)
        else
          checkPlan()
        waitUntilCompleted0()
      }
    }
    waitUntilCompleted0()
    this.synchronized{
      if(failure.isDefined)
        throw new RuntimeException("Exception in parallel processor", failure.get)
    }
  }

  def runUntilAllOutputSignals:List[Signal[_]] = {
    waitUntilCompleted()
    this.synchronized{ // access to internals should be synchronized anyway
      outputs.sorted.map(_.value)
    }
  }
}


object ComputationState {
  def runtimeSystemToTotalTrellisProducer(implicit ec:ExecutionContext) =
    (rs:RuntimeSystem) =>
    (context:Context, signal:Signal[_]) => {
      val cs = new ComputationState(rs, context)
      cs.addSignal(AtTime(HTime(None, 0), signal))
      val signals = cs.runUntilAllOutputSignals
      val newContext = cs.getContext
      (newContext, signals)
    }

  implicit class RichStaticSystem(system:StaticSystem)(implicit ec:ExecutionContext) {
    /** Converts a system to a parallel RuntimeSystem.
      * Actually, converts inner subsystems to ParallelRuntimeSystem.
      */
    def toParallelRuntimeSystem:RuntimeSystem = {
      SystemConverting.toRuntimeSystem(system, system.outputContacts, runtimeSystemToTotalTrellisProducer)
    }

    def toParallelSimpleSignalProcessor:SimpleSignalProcessor =
      runtimeSystemToTotalTrellisProducer(ec)(toParallelRuntimeSystem).toSimpleSignalProcessor(system.s0)
    /**
     * The resulting Dynamic system will work in background. However,
     * it will block until all computations have been completed.
     */
    def toParallelDynamicSystem:DynamicSystem =
      DynamicSystem(system.inputContacts, system.outputContacts, system.name,
        toParallelSimpleSignalProcessor)

  }

}
