///////////////////////////////////////////////////////////////
// СинаптическаяСеть
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
 * Created: 18.03.2013
 */
package ru.primetalk.synapse.core

import scala.language.existentials
import scala.Predef._
import scala.util.Try

/** This contact is used to enable special simultaneous processing of signals. */
object TrellisContact extends Contact[List[Signal[_]]]

case class SubsystemDirectSignal(subsystemName:String, signal:Signal[_])
/** This contact is used to process signals of internal system*/
object SubsystemSpecialContact extends Contact[SubsystemDirectSignal]

case class TrellisProducerSpeedy(name:String,
                                  signalProcessors: Map[Contact[_], List[SingleSignalProcessor]],
                                  stopContacts: Set[Contact[_]])
  extends TrellisProducer {
  def apply(t: TrellisElement): TrellisElement = {
    val signals = t._2
    //			val (outputs, inners) = t._2.partition(s ⇒ system.isOutputContact(s._1))
    val toProcess = new Signal(TrellisContact, signals) :: signals //inners

    var newState = t._1
    var newSignals = List[Signal[_]]() // : Signals
    for {
      signal ← toProcess
      c = signal.contact
    } if (stopContacts.contains(c))
      newSignals = signal :: newSignals
    else
      for (proc ← signalProcessors(c)) {
        try {
          val (ctx, signals) = proc.apply(newState, signal)
          newState = ctx
          newSignals = signals reverse_::: newSignals
        } catch {
          case e: Exception => throw new RuntimeException(
		        s"Exception ${e.getClass.getSimpleName} in handler during processing '$signal' in system '$name'.",e)
        }
      }

    (newState, newSignals.reverse)
  }
}
/** Generates trellis until there are some data on nonStop contacts.
  * Can also process signals from child subsystems (not constrained only to input contacts.*/
case class TrellisProducerLoopy(trellisProducer: TrellisProducer,
                                stopContacts: Set[Contact[_]]) extends SingleSignalProcessor {
	private def from(t0: TrellisElement): Stream[TrellisElement] =
		t0 #:: from(trellisProducer(t0))

	def apply(context: Context, signal: Signal[_]): TrellisElement =
		from((context, List(signal))).filter(t ⇒ (t._2.map(_._1).toSet -- stopContacts).isEmpty).head
}

/**
 * Processes signals for the given system.
 * @author А.Жижелев
 *
 */
class SignalProcessorOld(system: StaticSystem,
                      inContacts: Set[Contact[_]], stopContacts: Set[Contact[_]])
	extends SingleSignalProcessor {
	val mapContactsToProcessors =
    SignalProcessing.systemToSignalProcessors(List(),system,
      SignalProcessing.componentToSignalProcessor)
	val step = TrellisProducerSpeedy(system.name, mapContactsToProcessors, stopContacts): TrellisProducer
	val processInnerSignals = TrellisProducerLoopy(step, stopContacts)


	def apply(context: Map[Contact[_], _], signal: Signal[_]): TrellisElement = {
		if (!inContacts.contains(signal.contact))
			throw new IllegalArgumentException(
				s"The system ${system.name} does not have appropriate input contacts for signal: $signal.")

		processInnerSignals(context, signal)
	}
}
/**
 * Processes signals for the given system.
 * @author А.Жижелев
 *
 */
class SignalProcessor(mapContactsToProcessors: Map[Contact[_], List[SingleSignalProcessor]],
											name:String,
                      inContacts: Set[Contact[_]], stopContacts: Set[Contact[_]])
	extends SingleSignalProcessor {

	val step = TrellisProducerSpeedy(name:String,mapContactsToProcessors, stopContacts): TrellisProducer

	val processInnerSignals = TrellisProducerLoopy(step, stopContacts)

  def assertIsInInputs(signal:Signal[_]){
    if (!inContacts.contains(signal.contact))
      throw new IllegalArgumentException(
        s"The system $name does not have appropriate input contacts for signal: $signal.")
  }
	def apply(context: Map[Contact[_], _], signal: Signal[_]): TrellisElement = {

		processInnerSignals(context, signal)
	}

}

object SignalProcessing {

  type SimpleComponentConverter = PartialFunction[Component, SingleSignalProcessor]
  type SubsystemConverter = PartialFunction[(List[String], StaticSystem, Component), SingleSignalProcessor]

  type ComponentConverter = (List[String], StaticSystem, Component) => SingleSignalProcessor
  implicit def enrichConverter(cvt:SimpleComponentConverter):SubsystemConverter = {
    case (_, _, comp) if cvt.isDefinedAt(comp) => cvt(comp)
  }

  class MutableComponentConverter extends ComponentConverter {
    private val subsystemConverters = new scala.collection.mutable.ListBuffer[SubsystemConverter]()
    private var readOnly = false
    private def assertWritable(arg:Any){
      if(readOnly)
        throw new IllegalStateException(s"MutableComponentConverter is read only. Cannot add $arg.")
    }

    def +=(cvt:SubsystemConverter) {
      assertWritable(cvt)
      subsystemConverters += cvt
    }

    def ++=(cvts:TraversableOnce[SubsystemConverter]) {
      assertWritable(cvts)
      subsystemConverters ++= cvts
    }

    lazy val totalConverter:SubsystemConverter = {
      readOnly = true
      (subsystemConverters :\ enrichConverter(unmatched))(_ orElse _)
    }

    def apply(path:List[String], system:StaticSystem, component:Component):SingleSignalProcessor = {
      val tuple = (path,system,component)
      val t = Try{
        totalConverter(tuple)
      }
      if(t.isSuccess)
        t.get
      else {
        val e = t.failed.get
        throw new RuntimeException(s"Cannot convert $tuple.", e)
      }
    }
  }

  val linkToSignalProcessor1 : SimpleComponentConverter = {
		case Link(from, to, MapLink(f, _)) ⇒
			(context, signal) ⇒
				(context, List(new Signal(to, f.asInstanceOf[Any ⇒ Any](signal.data))))
		case Link(from, to, FlatMapLink(f, _)) ⇒ {
			(context, signal) ⇒
				val fun = f.asInstanceOf[Any ⇒ TraversableOnce[Any]]
				val res = fun(signal.data)
				(context, res.map(new Signal(to, _)).toList)
		}
		case Link(from, to, StateZipLink(pe: Contact[_], _)) ⇒ {
			(context, signal) ⇒
				val stateHandle = pe: Contact[_]
				(context, List(new Signal(to, (context(stateHandle), signal.data))))
		}
		case Link(from, to, NopLink(_)) ⇒
			(context, signal) ⇒
				(context, List(new Signal(to, signal.data)))
		// Deprecated. Use StateZipLink
		case Link(from, to, StatefulMapLink(f, pe, _)) ⇒
			(context, signal) ⇒ {
				val stateHandle = pe: Contact[_]
				val oldState = context(stateHandle)
				val (nState, nData) = f.asInstanceOf[(Any, Any) ⇒ (Any, Any)](oldState, signal.data)
				(context updated(stateHandle, nState), List(new Signal(to, nData)))
			}
		// Deprecated. Use StateZipLink
		case Link(from, to, StatefulFlatMapLink(f, pe, _)) ⇒
			(context, signal) ⇒ {
				val stateHandle = pe: Contact[_]
				val oldState = context(stateHandle)
				val (nState, nDataSeq) = f.asInstanceOf[(Any, Any) ⇒ (Any, Seq[Any])](oldState, signal.data)
				(context updated(stateHandle, nState), nDataSeq.toList.map(new Signal(to, _)))
			}
		case StateUpdate(from, pe, _, f) ⇒ {
			(context, signal) ⇒
				val stateHandle = pe: Contact[_]
				(context updated(stateHandle, f.asInstanceOf[(Any, Any) => Any](context(stateHandle), signal.data)), List())
		}
  }

  def innerSystemSignalHandlerWithoutShared(subsystemStateHandle1:Contact[_], proc:SignalProcessor)(context: Context, signal:Signal[_]) = {
    val oldState = context(subsystemStateHandle1).asInstanceOf[Map[Contact[_], _]]
    val oldStateWithShared = oldState
    val (newState, signals) = proc(oldStateWithShared, signal)
    val newStateWithoutShared = newState
    (context updated(subsystemStateHandle1, newStateWithoutShared), signals)
  }
  def innerSystemSignalHandlerWithShared(
                                         subsystemStateHandle1:Contact[_],
                                         proc:SignalProcessor,
                                         sharedStateHandles: List[StateHandle[_]]) = {
    val sharedStateHandlersSet = sharedStateHandles.toSet[Contact[_]]
    (context: Context, signal:Signal[_]) =>
    val oldState = context(subsystemStateHandle1).asInstanceOf[Map[Contact[_], _]]
    val sharedStates = sharedStateHandles.map(ssh ⇒ (ssh, context(ssh)))
    val oldStateWithShared = oldState ++ sharedStates
    val (newState, signals) = proc(oldStateWithShared, signal)
    val newStateWithoutShared = newState.filterKeys(ssh ⇒ !sharedStateHandlersSet.contains(ssh))
    val sharedStateValues = newState.filterKeys(ssh ⇒ sharedStateHandlersSet.contains(ssh))
    ((context updated(subsystemStateHandle1, newStateWithoutShared)) ++ sharedStateValues, signals)
  }

  def innerSystemToSignalProcessor(converterRecursive: ComponentConverter): SubsystemConverter = {
    case (path, _, InnerSystem(subsystem, subsystemStateHandle, sharedStateHandles)) ⇒
      val mapContactsToProcessors =
        SignalProcessing.systemToSignalProcessors(subsystem.name::path,
          subsystem,
          converterRecursive)
      val proc = new SignalProcessor(mapContactsToProcessors, subsystem.name, subsystem.inputContacts, subsystem.outputContacts)
      (
        if (sharedStateHandles.isEmpty)
          innerSystemSignalHandlerWithoutShared(subsystemStateHandle, proc)
        else
          innerSystemSignalHandlerWithShared(subsystemStateHandle, proc, sharedStateHandles)
      ): SingleSignalProcessor
  }

  def redLinkToSignalProcessor(converterWithoutRedLinks:ComponentConverter) : SubsystemConverter = {
		case (path, system, Link(from, to, RedMapLink(stopContacts, label))) ⇒

			val mapContactsToProcessors =
        SignalProcessing.systemToSignalProcessors(path, system,
          converterWithoutRedLinks
        )
			val proc = new SignalProcessor(mapContactsToProcessors, system.name+"RedMapLink("+label+")", Set(to), stopContacts)
			(context, signal) ⇒ proc(context, Signal(to, signal.data))
	}
  val redLinkDummy:SubsystemConverter = {
    case (path, system, Link(from, to, RedMapLink(stopContacts, label))) ⇒
      (context, signal) ⇒ (context, List())
  }

  val unmatched : SimpleComponentConverter = {
    case component =>
      throw new IllegalArgumentException(
        s"The component $component cannot be converted to SingleSignalProcessor.")
  }

	/** Converts components to a function that will do the work when the data appears on one of the contacts. */
	def componentToSignalProcessor : MutableComponentConverter = {
    val combinedConverter = new MutableComponentConverter
    val converterWithoutRedLinks = new MutableComponentConverter
    combinedConverter += redLinkToSignalProcessor(converterWithoutRedLinks)
    combinedConverter += linkToSignalProcessor1
    converterWithoutRedLinks += linkToSignalProcessor1
    val inner = innerSystemToSignalProcessor(combinedConverter )
    combinedConverter += inner
    converterWithoutRedLinks += inner
    converterWithoutRedLinks += redLinkDummy
    combinedConverter
  }

  def subsystemDirectProcessor(procs:Map[String, SingleSignalProcessor]):SingleSignalProcessor = {
    case (context, outerSignal) =>
      outerSignal match {
        case Signal(SubsystemSpecialContact, SubsystemDirectSignal(subsystemName, signal)) ⇒
          procs.get(subsystemName).
            map(proc => proc(context, signal)).
            getOrElse((context, List()))
        case _ =>
          throw new IllegalArgumentException(s"Wrong data on contact SubsystemSpecialContact: $outerSignal.")
      }
  }
	def systemToSignalProcessors( path:List[String],
                                system: StaticSystem,
                                converter: ComponentConverter): Map[Contact[_], List[SingleSignalProcessor]] = {

    val processors = for {
      component ← system.components
      proc = converter(path, system, component):SingleSignalProcessor
    } yield (component, proc)
		val contactsProcessors = (
			for {
        (component, proc) ← processors
				i ← component.inputContacts
			} yield (i, proc): (Contact[_], SingleSignalProcessor) // unify existential types within pair.
			).toList
    val innerSystems =
      processors.collect{
        case (comp:ComponentWithInternalStructure, proc) => (comp.toStaticSystem.name, proc)
      }
    val contactsProcessors2 =
      if(innerSystems.isEmpty)
        contactsProcessors
      else
        (SubsystemSpecialContact,subsystemDirectProcessor(innerSystems.toMap)) :: contactsProcessors //(p => (SubsystemSpecialContact, p))


		val lst = contactsProcessors2.groupBy(_._1).map(p ⇒ (p._1, p._2.map(_._2)))
		val signalProcessors = lst.toMap[Contact[_], List[SingleSignalProcessor]].withDefault(c ⇒ List())
		signalProcessors
	}

	def toDynamicSystem(path:List[String], system: StaticSystem) = {
		val mapContactsToProcessors =
      SignalProcessing.systemToSignalProcessors(path, system,
        SignalProcessing.componentToSignalProcessor)
		val proc = new SignalProcessor(mapContactsToProcessors, system.name, system.inputContacts, system.outputContacts)
    var state = system.s0
    def receive(signal: Signal[_]): List[Signal[_]] = {
      def receive0(st: system.StateType, resSignals: List[Signal[_]], signals: List[Signal[_]]): (system.StateType, List[Signal[_]]) = signals match {
        case Nil ⇒ (st, resSignals)
        case head :: tail ⇒
          val (newState, newSignals) = proc(st, head)
          receive0(newState, newSignals reverse_::: resSignals, tail)
      }
      val result = receive0(state, Nil, signal :: Nil)
      state = result._1
      result._2.reverse
    }
    new DynamicSystem(system.inputContacts, system.outputContacts, system.name, receive)
  }

}

//    type TrellisElement = (Map[Contact[_], _], List[Signal[_]])
//		private def stepLegacy(t : (Map[Contact[_], _], List[Signal[_]])) : (Map[Contact[_], _], List[Signal[_]]) = {
//			def step0(contextAndResSignals : (Map[Contact[_], _], List[Signal[_]]), task : (InnerSignalProcessor, Signal[_])) : (Map[Contact[_], _], List[Signal[_]]) = {
//				val (proc, signal) = task
//				val (context, resSignals) = contextAndResSignals
//				val result = proc(context, signal)
//				(result._1, result._2 reverse_::: resSignals)
//			}
//			val (context, signals) = t
//			val (outputs, inners) = signals.partition(s ⇒ system.isOutputContact(s._1))
//			val toProcess = new Signal(TrellisContact, signals) :: inners
//			/** Формирует "задания на вычисления" — совокупность компонента и данных. */
//			val processingTasks = for {
//				signal ← toProcess
//				c = signal.contact
//				proc <- mapContactsToProcessors(c)
//			} yield (proc, signal : Signal[_])
//
//			val newSignalsAccumulator = List[Signal[_]]() // : Signals
//			val (newState, newSignals) = ((context, newSignalsAccumulator) /: processingTasks) (step0)    // == processingTasks foldLeft (context, newSignalsAccumulator)
//			(newState, newSignals reverse_::: outputs)
//		}
//    /** This version of step has slightly better performance than the previous one. It decreases the number of intermediate objects created. */
//    private def stepSpeedy(t: TrellisElement): TrellisElement = {
//      val signals = t._2
//      //			val (outputs, inners) = t._2.partition(s ⇒ system.isOutputContact(s._1))
//      val toProcess = new Signal(TrellisContact, signals) :: signals //inners
//
//      var newState = t._1
//      var newSignals = List[Signal[_]]() // : Signals
//      for {
//        signal ← toProcess
//        c = signal.contact
//      } if (stopContacts.contains(c))
//        newSignals = signal :: newSignals
//      else
//        for (proc ← mapContactsToProcessors(c)) {
//          try {
//            val (ctx, signals) = proc.apply(newState, signal)
//            newState = ctx
//            newSignals = signals reverse_::: newSignals
//          } catch {
//            case e: Exception => throw new RuntimeException(s"Exception ${e.getClass.getSimpleName} in handler during processing $signal.")
//          }
//        }
//
//      (newState, newSignals.reverse)
//    }

//    private def from(t0: TrellisElement): Stream[TrellisElement] =
//      t0 #:: from(step(t0))
//    /** Can process signals from child subsystems. */
//    def processInnerSignals(context: Map[Contact[_], _], signal: Signal[_]): (Map[Contact[_], _], List[Signal[_]]) = {
//      //			from((context, List(signal))).filter(t ⇒ (t._2.map(_._1).toSet.intersect(processedContacts)).isEmpty).head
//      from((context, List(signal))).filter(t ⇒ (t._2.map(_._1).toSet -- stopContacts).isEmpty).head
//    }
