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
		case Link(from, to, FlatMapLink(f, name)) ⇒
      RuntimeComponentFlatMap(name, from, to,
      {(signal) ⇒
          val fun = f.asInstanceOf[Any ⇒ TraversableOnce[Any]]
          val res = fun(signal.data)
          res.map(new Signal(to, _)).toList
      })
    case Link(from, to, StatefulFlatMapLink(f, pe, name)) ⇒
      RuntimeComponentStateFlatMap[Any](name, List(from), List(to), pe,
      {(value, signal) ⇒
        val fun = f.asInstanceOf[(Any, Any) ⇒ (Any, Seq[Any])]
        val (nState, nDataSeq) = fun(value, signal.data)
        (nState, nDataSeq.toList.map(new Signal(to, _)))
      })
    case Link(from, to, NopLink(name)) ⇒
      RuntimeComponentFlatMap(name, from, to,
      {(signal) ⇒
        List(new Signal(to, signal.data))
      })
		case Link(from, to, StateZipLink(pe, name)) ⇒
			RuntimeComponentStateFlatMap[Any](name, List(from), List(to), pe,
      {(value, signal) ⇒
				(value, List(new Signal(to, (value, signal.data))))
      })
		case StateUpdate(from, pe, name, f) ⇒
      RuntimeComponentStateFlatMap[Any](name, List(from), List(), pe,
        {(value, signal) ⇒
          val result = f.asInstanceOf[(Any, Any) => Any](value, signal.data)
          (result, List())
        })
  }

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

      ((context updated(subsystemStateHandle1, newStateWithoutShared)) ++ sharedStateValues, signals)
    }
  }

  def innerSystemToSignalProcessor(converterRecursive: ComponentConverter, rsToTtp:RuntimeSystemToTotalTrellisProducerConverter): SubsystemConverter = {
    case (path, _, InnerSystem(subsystem, subsystemStateHandle, Nil)) ⇒
      val rs = systemToRuntimeSystem(subsystem.name::path,
        subsystem,
        converterRecursive,
        subsystem.outputContacts)
      val proc = rsToTtp(rs)//rs.toTotalTrellisProducer
      RuntimeComponentStateFlatMap(subsystem.name, subsystem.inputs, subsystem.outputs, subsystemStateHandle,proc)
    case (path, _, InnerSystem(subsystem, subsystemStateHandle, sharedStateHandles)) ⇒
      val rs = systemToRuntimeSystem(subsystem.name::path,
        subsystem,
        converterRecursive,
        subsystem.outputContacts)
      val proc = rsToTtp(rs)//rs.toTotalTrellisProducer
      val stateHandles = subsystemStateHandle :: sharedStateHandles
      RuntimeComponentMultiState(subsystem.name, stateHandles,innerSystemSignalHandlerWithShared(subsystemStateHandle, proc, sharedStateHandles))
  }

  def redLinkToSignalProcessor(converterWithoutRedLinks:ComponentConverter) : SubsystemConverter = {
		case (path, system, Link(from, to, RedMapLink(stopContacts, label))) ⇒
      val rs = systemToRuntimeSystem(path,system, converterWithoutRedLinks, stopContacts)
      val proc = rs.toTotalTrellisProducer// toRuntimeComponentHeavy(system.privateStateHandles)
			RuntimeComponentMultiState(system.name, system.privateStateHandles, (context, signal) ⇒ proc(context, Signal(to, signal.data)))
	}
  val redLinkDummy:SubsystemConverter = {
    case (path, system, Link(from, to, RedMapLink(stopContacts, label))) ⇒
      RuntimeComponentMultiState(system.name, system.privateStateHandles, (context, signal) ⇒ (context, List()))
  }

  val unmatched : SubsystemConverter = {
    case (path, system, component) =>
      throw new IllegalArgumentException(
        s"The component $component cannot be converted to SingleSignalProcessor (${(path, system, component)}).")
  }

	/** Converts components to a function that will do the work when the data appears on one of the contacts. */
	def componentToSignalProcessor(rsToTtp:RuntimeSystemToTotalTrellisProducerConverter) : MutableComponentConverter = {
    val combinedConverter = new MutableComponentConverter
    val inner = innerSystemToSignalProcessor(combinedConverter, rsToTtp )

    val converterWithoutRedLinks = new MutableComponentConverter
    converterWithoutRedLinks += redLinkDummy
    converterWithoutRedLinks += linkToSignalProcessor1
    converterWithoutRedLinks += inner

    combinedConverter += redLinkToSignalProcessor(converterWithoutRedLinks)
    combinedConverter += linkToSignalProcessor1
    combinedConverter += inner
    combinedConverter
  }

  // TODO: replace with dispatching red links
  def subsystemDirectProcessor(procs:Map[String, RuntimeComponent]):RuntimeComponent =
    RuntimeComponentMultiState("subsystemDirectProcessor", List(), {
      case (context, outerSignal) =>
        outerSignal match {
          case Signal(SubsystemSpecialContact, SubsystemDirectSignal(subsystemName, signal)) ⇒
            procs.get(subsystemName).
              map {
                case RuntimeComponentMultiState(_, _, f) =>
                  f(context, signal)
                case RuntimeComponentFlatMap(_, _, _, f) =>
                  val r = f(signal)
                  (context, r)
                case RuntimeComponentStateFlatMap(_, _, _, sh, f) =>
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
	}

  def toRuntimeSystem(system:StaticSystem,
                    //inContacts: Set[Contact[_]],
                      stopContacts: Set[Contact[_]],
                       rsToTtp:RuntimeSystemToTotalTrellisProducerConverter):RuntimeSystem = {
    componentToSignalProcessor(rsToTtp).convertToRuntimeSystem(system, stopContacts)
  }
  def toSimpleSignalProcessor(path:List[String],
                              system: StaticSystem,
                              rsToTtp:RuntimeSystemToTotalTrellisProducerConverter):SimpleSignalProcessor = {
    /** The state of the system. */
    var state = system.s0

    val rs = systemToRuntimeSystem(path,system,
      componentToSignalProcessor(rsToTtp), system.outputContacts)
    val proc = rsToTtp(rs)// rs.toTotalTrellisProducer

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
    receive
  }
	def toDynamicSystem(path:List[String],
                      system: StaticSystem,
                      rsToTtp:RuntimeSystemToTotalTrellisProducerConverter) =
    new DynamicSystem(system.inputContacts, system.outputContacts, system.name, toSimpleSignalProcessor(path, system, rsToTtp))


}
