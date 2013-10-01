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
 * Created: 07.08.13, zhizhelev
 */
package ru.primetalk.synapse.core

import scala.util.Try
import scala.language.implicitConversions
import scala.language.existentials

/**
  * Toolkit for conversion of StaticSystem to RuntimeSystems.
  * The conversion purpose is to convert all Components of a StaticSystem
  * to some kind of Signal processors.
  */
object SystemConverting {

  type SimpleComponentConverter = PartialFunction[Component, RuntimeComponent]
  type SubsystemConverter = PartialFunction[
    (List[String],
      StaticSystem,
      Component),
    RuntimeComponent]
  type ComponentConverter = (List[String], StaticSystem, Component) => RuntimeComponent

  implicit def enrichConverter(cvt:SimpleComponentConverter):SubsystemConverter = {
    case (_, _, comp) if cvt.isDefinedAt(comp) =>
      val rc = cvt(comp)
    rc
//        (ctx, signals) => {
//          val (upd, newSignals) = rc.f(ctx, signals)
//          ( (ctx /: upd)(_ + _), newSignals)
//        }
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
    }
  }

  val linkToSignalProcessor1 : SimpleComponentConverter = {
		case Link(from, to, FlatMapLink(f, _)) ⇒
      RuntimeComponentFlatMap(List(from), List(to), {
        (signal) ⇒
          val fun = f.asInstanceOf[Any ⇒ TraversableOnce[Any]]
          val res = fun(signal.data)
          res.map(new Signal(to, _)).toList
      })
    case Link(from, to, StatefulFlatMapLink(f, pe, _)) ⇒
      RuntimeComponentStateFlatMap[Any](List(from), List(to), pe, {(value, signal) ⇒
        val fun = f.asInstanceOf[(Any, Any) ⇒ (Any, Seq[Any])]
        val (nState, nDataSeq) = fun(value, signal.data)
        (nState, nDataSeq.toList.map(new Signal(to, _)))
      })
    case Link(from, to, NopLink(_)) ⇒
      RuntimeComponentFlatMap(List(from), List(to), {(signal) ⇒
        List(new Signal(to, signal.data))
      })
		case Link(from, to, StateZipLink(pe, _)) ⇒
			RuntimeComponentStateFlatMap[Any](List(from), List(to), pe, {(value, signal) ⇒
				(value, List(new Signal(to, (value, signal.data))))
      })
		case StateUpdate(from, pe, _, f) ⇒ RuntimeComponentStateFlatMap[Any](List(from), List(), pe, {
      (value, signal) ⇒
        val result = f.asInstanceOf[(Any, Any) => Any](value, signal.data)
        (result, List())
		})
  }

//  def innerSystemSignalHandlerWithoutShared(
//          subsystemStateHandle1:Contact[_],
//          proc:TotalTrellisProducer
//          )(context: Context,
//          signal:Signal[_]):(Context, List[Signal[_]]) =
//  {
//    val oldState = context(subsystemStateHandle1).asInstanceOf[Map[Contact[_], _]]
//    val (newState, signals) = proc(oldState, signal)
//    (oldState.updated(subsystemStateHandle1, newState), signals)
//  }
//
  def innerSystemSignalHandlerWithShared(
                                         subsystemStateHandle1:Contact[_],
                                         proc:TotalTrellisProducer,
                                         sharedStateHandles: List[Contact[_]]) =
  {
    val sharedStateHandlersSet = sharedStateHandles.toSet[Contact[_]]

    {(context: Context, signal:Signal[_]) =>
      val oldState = context(subsystemStateHandle1).asInstanceOf[Map[Contact[_], _]]
      val sharedStates = sharedStateHandles.map(ssh ⇒ (ssh, context(ssh)))
      val oldStateWithShared = oldState ++ sharedStates
      val (newChildContext:Context, signals) = proc(oldStateWithShared, signal)
      val (sharedStateValues, newStateWithoutShared) =
        newChildContext.toList.partition{ case (sh, _) ⇒ sharedStateHandlersSet.contains(sh)}

  //    val newStateWithoutShared = delta.filter(ssh ⇒ !sharedStateHandlersSet.contains(ssh._1))
  //    val sharedStateValues = delta.filter(ssh ⇒ sharedStateHandlersSet.contains(ssh._1))

      ((context updated(subsystemStateHandle1, newStateWithoutShared)) ++ sharedStateValues, signals)
    }
  }

  def innerSystemToSignalProcessor(converterRecursive: ComponentConverter): SubsystemConverter = {
    case (path, _, InnerSystem(subsystem, subsystemStateHandle, Nil)) ⇒
      val rs = systemToRuntimeSystem(subsystem.name::path,
        subsystem,
        converterRecursive,
        subsystem.outputContacts)
      val proc = rs.toTotalTrellisProducer
      RuntimeComponentStateFlatMap(subsystem.inputs, subsystem.outputs, subsystemStateHandle,proc)
//        {(subsystemContext: Context, signal:Signal[_]) =>
//          if(!context.contains(subsystemStateHandle.asInstanceOf[Contact[_]]))
//            throw new IllegalArgumentException(s"The context $context does not contain system state under the key $subsystemStateHandle")
//          val oldStateTry = Try{context(subsystemStateHandle.asInstanceOf[Contact[_]]).asInstanceOf[Map[Contact[_], _]]}
//          if(oldStateTry.isFailure)
//            throw new IllegalArgumentException(
//              s"The context $context does not contain system state under the key $subsystemStateHandle",
//              oldStateTry.failed.get)
//          val oldState = oldStateTry.get
//          val (newState, signals) = proc(subsystemContext, signal)
//          (oldState.updated(subsystemStateHandle.asInstanceOf[Contact[_]], newState), signals)
//        })
//        innerSystemSignalHandlerWithoutShared(subsystemStateHandle, proc))
    case (path, _, InnerSystem(subsystem, subsystemStateHandle, sharedStateHandles)) ⇒
      val rs = systemToRuntimeSystem(subsystem.name::path,
        subsystem,
        converterRecursive,
        subsystem.outputContacts)
      val proc = rs.toTotalTrellisProducer
      val stateHandles = subsystemStateHandle :: sharedStateHandles
      RuntimeComponentMultiState(stateHandles,innerSystemSignalHandlerWithShared(subsystemStateHandle, proc, sharedStateHandles))
  }

  def redLinkToSignalProcessor(converterWithoutRedLinks:ComponentConverter) : SubsystemConverter = {
		case (path, system, Link(from, to, RedMapLink(stopContacts, label))) ⇒
      val rs = systemToRuntimeSystem(path,system, converterWithoutRedLinks, stopContacts)
      val proc = rs.toTotalTrellisProducer// toRuntimeComponentHeavy(system.privateStateHandles)

//			val mapContactsToProcessors =
//        systemToSignalProcessors(path, system,
//          converterWithoutRedLinks
//        )
//      val rs = RuntimeSystem(system.name+"RedMapLink("+label+")", mapContactsToProcessors, stopContacts)
//			val proc = rs.toRuntimeComponent//new SignalProcessor(rs, Set(to))
			RuntimeComponentMultiState(system.privateStateHandles, (context, signal) ⇒ proc(context, Signal(to, signal.data)))
	}
  val redLinkDummy:SubsystemConverter = {
    case (path, system, Link(from, to, RedMapLink(stopContacts, label))) ⇒
      RuntimeComponentMultiState(system.privateStateHandles, (context, signal) ⇒ (context, List()))
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

  def subsystemDirectProcessor(procs:Map[String, RuntimeComponent]):RuntimeComponent =
    RuntimeComponentMultiState(List(), {
      case (context, outerSignal) =>
        outerSignal match {
          case Signal(SubsystemSpecialContact, SubsystemDirectSignal(subsystemName, signal)) ⇒
            procs.get(subsystemName).
              map {
                case RuntimeComponentMultiState(_, f) =>
                  f(context, signal)
                case RuntimeComponentFlatMap(_, _, f) =>
                  val r = f(signal)
                  (context, r)
                case RuntimeComponentStateFlatMap(_, _, sh, f) =>
                  val s = context(sh)
                  val r = f(s, signal)
                  (context.updated(sh, r._1), r._2)
              }.
                getOrElse((context, List()))
          case _ =>
            throw new IllegalArgumentException(s"Wrong data on contact SubsystemSpecialContact: $outerSignal.")
        }
    } )
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
    val proc = rs.toTotalTrellisProducer
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
