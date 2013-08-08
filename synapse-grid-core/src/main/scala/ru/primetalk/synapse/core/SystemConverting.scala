///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * ${PROJECT_NAME}
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 *
 * Created: 07.08.13, zhizhelev
 */
package ru.primetalk.synapse.core

import scala.util.Try
import scala.language.implicitConversions

/**
  * Toolkit for conversion of StaticSystem to RuntimeSystems.
  * The conversion purpose is to convert all Components of a StaticSystem
  * to some kind of Signal processors.
  */
object SystemConverting {

  type SimpleComponentConverter = PartialFunction[Component, RuntimeComponent]
  type SubsystemConverter = PartialFunction[(List[String], StaticSystem, Component), RuntimeComponent]
  type ComponentConverter = (List[String], StaticSystem, Component) => RuntimeComponent

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
      (subsystemConverters :\ unmatched)(_ orElse _)
    }

    def apply(path:List[String], system:StaticSystem, component:Component):RuntimeComponent = {
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
    def convertToRuntimeSystem(system:StaticSystem,
                               stopContacts: Set[Contact[_]]):RuntimeSystem = {
      systemToRuntimeSystem(List(),system, this, stopContacts)
//      val mapContactsToProcessors =
//        systemToSignalProcessors(List(),system, this)
//      RuntimeSystem(system.name, mapContactsToProcessors, stopContacts)
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

  def innerSystemSignalHandlerWithoutShared(subsystemStateHandle1:Contact[_], proc:RuntimeComponent)(context: Context, signal:Signal[_]) = {
    val oldState = context(subsystemStateHandle1).asInstanceOf[Map[Contact[_], _]]
    val oldStateWithShared = oldState
    val (newState, signals) = proc(oldStateWithShared, signal)
    val newStateWithoutShared = newState
    (context updated(subsystemStateHandle1, newStateWithoutShared), signals)
  }
  def innerSystemSignalHandlerWithShared(
                                         subsystemStateHandle1:Contact[_],
                                         proc:RuntimeComponent,
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
      //      val mapContactsToProcessors =
      //        systemToSignalProcessors(subsystem.name::path,
      //          subsystem,
      //          converterRecursive)
      //      val proc = RuntimeSystem(subsystem.name, mapContactsToProcessors, subsystem.outputContacts).toRuntimeComponent
      val rs = systemToRuntimeSystem(subsystem.name::path,subsystem, converterRecursive, subsystem.outputContacts)
      val proc = rs.toRuntimeComponent
      (
        if (sharedStateHandles.isEmpty)
          innerSystemSignalHandlerWithoutShared(subsystemStateHandle, proc)
        else
          innerSystemSignalHandlerWithShared(subsystemStateHandle, proc, sharedStateHandles)
      ): RuntimeComponent
  }

  def redLinkToSignalProcessor(converterWithoutRedLinks:ComponentConverter) : SubsystemConverter = {
		case (path, system, Link(from, to, RedMapLink(stopContacts, label))) ⇒
      val rs = systemToRuntimeSystem(path,system, converterWithoutRedLinks, stopContacts)
      val proc = rs.toRuntimeComponent

//			val mapContactsToProcessors =
//        systemToSignalProcessors(path, system,
//          converterWithoutRedLinks
//        )
//      val rs = RuntimeSystem(system.name+"RedMapLink("+label+")", mapContactsToProcessors, stopContacts)
//			val proc = rs.toRuntimeComponent//new SignalProcessor(rs, Set(to))
			(context, signal) ⇒ proc(context, Signal(to, signal.data))
	}
  val redLinkDummy:SubsystemConverter = {
    case (path, system, Link(from, to, RedMapLink(stopContacts, label))) ⇒
      (context, signal) ⇒ (context, List())
  }

  val unmatched : SubsystemConverter = {
    case (path, system, component) =>
      throw new IllegalArgumentException(
        s"The component $component cannot be converted to SingleSignalProcessor (${(path, system, component)}).")
  }

	/** Converts components to a function that will do the work when the data appears on one of the contacts. */
	def componentToSignalProcessor : MutableComponentConverter = {
    val combinedConverter = new MutableComponentConverter
    val inner = innerSystemToSignalProcessor(combinedConverter )

    val converterWithoutRedLinks = new MutableComponentConverter
    converterWithoutRedLinks += redLinkDummy
    converterWithoutRedLinks += linkToSignalProcessor1
    converterWithoutRedLinks += inner

    combinedConverter += redLinkToSignalProcessor(converterWithoutRedLinks)
    combinedConverter += linkToSignalProcessor1
    combinedConverter += inner
    combinedConverter
  }

  def subsystemDirectProcessor(procs:Map[String, RuntimeComponent]):RuntimeComponent = {
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
	def systemToRuntimeSystem( path:List[String],
                                system: StaticSystem,
                                converter: ComponentConverter,
                                stopContacts:Set[Contact[_]]):RuntimeSystem = {
//  Map[Contact[_], List[RuntimeComponent]] = {

    val processors = for {
      component ← system.components
      proc = converter(path, system, component):RuntimeComponent
    } yield (component, proc)
		val contactsProcessors = (
			for {
        (component, proc) ← processors
				i ← component.inputContacts
			} yield (i, proc): (Contact[_], RuntimeComponent) // unify existential types within pair.
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
		val signalProcessors = lst.toMap[Contact[_], List[RuntimeComponent]].withDefault(c ⇒ List())

    RuntimeSystem(system.name, signalProcessors, stopContacts)

//    signalProcessors
	}

  def toRuntimeSystem(system:StaticSystem,
                    //inContacts: Set[Contact[_]],
                      stopContacts: Set[Contact[_]]):RuntimeSystem = {
    componentToSignalProcessor.convertToRuntimeSystem(system, stopContacts)
  }
	def toDynamicSystem(path:List[String], system: StaticSystem) = {
    /** The state of the system. */
    var state = system.s0

    val rs = systemToRuntimeSystem(path,system, componentToSignalProcessor, system.outputContacts)
    val proc = rs.toRuntimeComponent
//		val mapContactsToProcessors =
//      systemToSignalProcessors(path, system,
//        componentToSignalProcessor)
//    val rs = RuntimeSystem(system.name, mapContactsToProcessors, system.outputContacts)
//    val proc = rs.toRuntimeComponent//new SignalProcessor(rs, system.inputContacts)
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
