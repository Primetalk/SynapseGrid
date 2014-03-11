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
object SystemConvertingSupport {

  /** A simple converter is for simple components like links that do not require
    * context description.
    * For complex components use ComponentDescriptor
    */
  type SimpleComponentConverter = PartialFunction[Component, RuntimeComponent]

  /** Describes component within a system.
    *
    * The path is used in hierarchical subsystem transformations.
    * The parentSystem is used in red-links processing.
    */
  case class ComponentDescriptor(component: Component, path: List[String] = List(), parentSystem: StaticSystem)

  /** ComponentDescriptorConverter is for complex components with internal structure. */
  type ComponentDescriptorConverter = PartialFunction[ComponentDescriptor, RuntimeComponent]

  /** Converts one partial function to another. */
  implicit def enrichConverter(cvt: SimpleComponentConverter): ComponentDescriptorConverter = {
    case ComponentDescriptor(comp, _, _) if cvt.isDefinedAt(comp) =>
      val rc = cvt(comp)
      rc
  }

  /** Converter proxy is used to construct functional converter with
    * recursive internal structure.
    *
    * The target can be assigned after the object was constructed.
    */
  class ComponentDescriptorConverterProxy extends ComponentDescriptorConverter {
    private var converter: Option[ComponentDescriptorConverter] = None

    def target = converter.get

    def target_=(t: ComponentDescriptorConverter) {
      if (converter.isDefined)
        throw new IllegalStateException("Cannot change the target of proxy.")
      converter = Some(t)
    }

    def isDefinedAt(t: ComponentDescriptor) =
      target.isDefinedAt(t)

    def apply(t: ComponentDescriptor): RuntimeComponent =
      converter.map(_(t)).getOrElse(throw new IllegalStateException("Proxy target is not set."))

  }

  /** A component converter that can be extended by adding more converters to it.
    */
  class MutableComponentConverterBuilder {
    private val converters = new scala.collection.mutable.ListBuffer[ComponentDescriptorConverter]()
    private var readOnly = false
    private def assertWritable(arg:Any){
      if(readOnly)
        throw new IllegalStateException(s"MutableComponentConverter is read only. Cannot add $arg.")
    }

    def +=(cvt: ComponentDescriptorConverter) {
      assertWritable(cvt)
      converters += cvt
    }

    def ++=(cvts: TraversableOnce[ComponentDescriptorConverter]) {
      assertWritable(cvts)
      converters ++= cvts
    }

    /** Joins all added converters as a chain of partial functions.
      * The end of chain is `unmatched` converter that throws an exception. */
    lazy val totalConverter: ComponentDescriptorConverter = {
      readOnly = true
      (converters :\ unmatched)(_ orElse _)
    }

  }


  class MutableComponentConverterBuilderOld extends MutableComponentConverterBuilder with ComponentDescriptorConverter {
    /** An alternative to `totalConverter` is to call MutableComponentConverterBuilder as a ComponentConverter. */
    def apply(tuple: ComponentDescriptor): RuntimeComponent = {
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

    def isDefinedAt(tuple: ComponentDescriptor) =
      totalConverter.isDefinedAt(tuple)

  }

  val unmatched: ComponentDescriptorConverter = {
    case ComponentDescriptor(component, path, system) =>
      throw new IllegalArgumentException(
        s"The component $component cannot be converted to SingleSignalProcessor (${(path, system, component)}).")
  }

}

object SystemConverting {

  import SystemConvertingSupport._

  def innerSystemSignalHandlerWithShared(subsystemStateHandle1: Contact[_],
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

  def innerSystemToSignalProcessor(converterRecursive: ComponentDescriptorConverter,
                                   rsToTtp: RuntimeSystemToTotalTrellisProducerConverter): ComponentDescriptorConverter = {
    case ComponentDescriptor(InnerSystem(subsystem, subsystemStateHandle, Nil), path, _) ⇒
      val rs = systemToRuntimeSystem(subsystem.name :: path,
        subsystem,
        converterRecursive,
        subsystem.outputContacts)
      val proc = rsToTtp(rs)//rs.toTotalTrellisProducer
      RuntimeComponentStateFlatMap(subsystem.name, subsystem.inputs, subsystem.outputs, subsystemStateHandle,proc)
    case ComponentDescriptor(InnerSystem(subsystem, subsystemStateHandle, sharedStateHandles), path, _) ⇒
      val rs = systemToRuntimeSystem(subsystem.name :: path,
        subsystem,
        converterRecursive,
        subsystem.outputContacts)
      val proc = rsToTtp(rs)//rs.toTotalTrellisProducer
      val stateHandles = subsystemStateHandle :: sharedStateHandles
      RuntimeComponentMultiState(subsystem.name, stateHandles,innerSystemSignalHandlerWithShared(subsystemStateHandle, proc, sharedStateHandles))
  }

  def redLinkToSignalProcessor(converterWithoutRedLinks: ComponentDescriptorConverter): ComponentDescriptorConverter = {
    case ComponentDescriptor(Link(from, to, RedMapLink(stopContacts, label)), path, system) ⇒
      val rs = systemToRuntimeSystem(path, system, converterWithoutRedLinks, stopContacts)
      val proc = rs.toTotalTrellisProducer// toRuntimeComponentHeavy(system.privateStateHandles)
			RuntimeComponentMultiState(system.name, system.privateStateHandles, (context, signal) ⇒ proc(context, Signal(to, signal.data)))
	}

  /** Ignores red links.
    * This converter is necessary to avoid infinite conversion of redlinks.
    */
  val redLinkDummy: ComponentDescriptorConverter = {
    case ComponentDescriptor(Link(from, to, RedMapLink(stopContacts, label)), path, system) ⇒
      RuntimeComponentMultiState(system.name, system.privateStateHandles, (context, signal) ⇒ (context, List()))
  }

  //  /** Converts components to a function that will do the work
  //    * when the data appears on one of the contacts.
  //    */
  //  def componentToSignalProcessor(rsToTtp: RuntimeSystemToTotalTrellisProducerConverter): MutableComponentConverterBuilderOld = {
  //    val combinedConverter = new MutableComponentConverterBuilderOld
  //    val inner = innerSystemToSignalProcessor(combinedConverter, rsToTtp )
  //
  //    val converterWithoutRedLinks = new MutableComponentConverterBuilderOld
  //    converterWithoutRedLinks += redLinkDummy
  //    converterWithoutRedLinks += RuntimeComponent.linkToRuntimeComponent
  //    converterWithoutRedLinks += inner
  //
  //    combinedConverter += redLinkToSignalProcessor(converterWithoutRedLinks)
  //    combinedConverter += RuntimeComponent.linkToRuntimeComponent
  //    combinedConverter += inner
  //    combinedConverter
  //  }


  /** Converts components to a function that will do the work
    * when the data appears on one of the contacts.
    *
    * Create special construction for processing red links: on top it converts redlinks
    * as a special components. Inside the special red-link component redLinks are
    * converted to dummies. So there will be no infinite loop in conversion.
    *
    * Also the conterter has a proxy-trick for subsystems. The same converter
    * is used to convert high level systems and inner subsystems.
    */
  def componentToSignalProcessor2(rsToTtp: RuntimeSystemToTotalTrellisProducerConverter, simpleConverters: ComponentDescriptorConverter*): ComponentDescriptorConverter = {
    val combinedConverter = new ComponentDescriptorConverterProxy

    val inner = innerSystemToSignalProcessor(combinedConverter, rsToTtp)

    val converterWithoutRedLinks = new MutableComponentConverterBuilder
    converterWithoutRedLinks += redLinkDummy
    //    converterWithoutRedLinks += RuntimeComponent.linkToRuntimeComponent
    converterWithoutRedLinks ++= simpleConverters
    converterWithoutRedLinks += inner


    val converterWithRedLinks = new MutableComponentConverterBuilder
    converterWithRedLinks += redLinkToSignalProcessor(converterWithoutRedLinks.totalConverter)
    //    converterWithRedLinks += RuntimeComponent.linkToRuntimeComponent
    converterWithRedLinks ++= simpleConverters
    converterWithRedLinks += inner

    combinedConverter.target = converterWithRedLinks.totalConverter
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


  def systemToRuntimeSystem(path: List[String],
                            system: StaticSystem,
                            converter: ComponentDescriptorConverter,
                            stopContacts:Set[Contact[_]]):RuntimeSystem = {
    val processors = for {
      component ← system.components
      proc = converter(ComponentDescriptor(component, path, system)): RuntimeComponent
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

    val m = componentToSignalProcessor2(rsToTtp, RuntimeComponent.linkToRuntimeComponent)

    systemToRuntimeSystem(List(), system, m, stopContacts)

  }

  def toSimpleSignalProcessor(path:List[String],
                              system: StaticSystem,
                              rsToTtp:RuntimeSystemToTotalTrellisProducerConverter):SimpleSignalProcessor = {
    /** The state of the system. */
    var state = system.s0

    val rs = systemToRuntimeSystem(path,system,
      componentToSignalProcessor2(rsToTtp, RuntimeComponent.linkToRuntimeComponent),
      system.outputContacts)
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

  def toDynamicSystem(path: List[String],
                      system: StaticSystem,
                      rsToTtp:RuntimeSystemToTotalTrellisProducerConverter) =
    new DynamicSystem(system.inputContacts, system.outputContacts, system.name, toSimpleSignalProcessor(path, system, rsToTtp))


}
