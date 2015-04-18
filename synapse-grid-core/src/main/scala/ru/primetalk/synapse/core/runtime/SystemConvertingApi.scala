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
package ru.primetalk.synapse.core.runtime

import ru.primetalk.synapse.core.components.InnerSystemComponent
import ru.primetalk.synapse.core.dsl.{ContactsIndexExt, ExceptionHandlingExt}

import scala.language.{existentials, implicitConversions}
import scala.util.Try

trait SystemConvertingApi extends RuntimeComponentApi
with TrellisApi with RuntimeSystemApi with TrellisProducerApi
with ContactsIndexExt with ExceptionHandlingExt{

  protected
  def debug(msg: =>String): Unit ={
    //println(msg)
  }
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

  /**
   * Toolkit for conversion of StaticSystem to RuntimeSystems.
   * The conversion purpose is to convert all Components of a StaticSystem
   * to some kind of Signal processors.
   *
   * As a basis for conversion Partial functions are used. They have convenient pattern-matching
   * syntax. Also partial functions allow easy chaining.
   * However, construction of composite converters is a bit tricky:
   * - ensure recursive conversion of subsystems;
   * - ensure red links conversion with the whole system in mind.
   * - extensible to add new component types.
   * It is possible to employ a slightly different approach. First get the class of a component and then
   * apply a partial function to the component. This approach however works only for simple one-level
   * components like links.
   *
   *
   */
  trait SystemConvertingSupport {

    /** Converter proxy is used to construct functional converter with
      * recursive internal structure.
      *
      * The target can be assigned after the object was constructed.
      * Thus a recursive structure becomes possible.
      */
    class ComponentDescriptorConverterProxy extends ComponentDescriptorConverter {
      private var converter: Option[ComponentDescriptorConverter] = None

      def target = converter.get

      def target_=(t: ComponentDescriptorConverter) {
        if (converter.isDefined)
          throw new IllegalStateException("Cannot change the target of a proxy.")
        converter = Some(t)
      }

      def isDefinedAt(t: ComponentDescriptor) =
        target.isDefinedAt(t)

      def apply(t: ComponentDescriptor): RuntimeComponent =
        converter.getOrElse(throw new IllegalStateException("Proxy target is not set."))(t)

    }

    /** A component converter that can be extended by adding more converters to it.
      */
    class MutableComponentConverterBuilder {
      private val converters = new scala.collection.mutable.ListBuffer[ComponentDescriptorConverter]()
      private var readOnly = false

      private def assertWritable(arg: Any) {
        if (readOnly)
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
        val t = Try {
          totalConverter(tuple)
        }
        if (t.isSuccess)
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


  object SystemConverting extends SystemConvertingSupport {

    /** Constructs a trellis producer for a subsystem.
      * Inner states of the system are stored in a single state that is
      * addressed with subsystemStateHandle1. There are also shared states
      * that are addressed with sharedStateHandles.
      * For shared handles their state is retrieved from outer context before calling
      * `proc` and then returned back after `proc`.
      * */
    def innerSystemSignalHandlerWithShared(subsystemStateHandle1: Contact[Context],
                                           proc: TotalTrellisProducer,
                                           sharedStateHandles: List[Contact[_]]):
    TotalTrellisProducer = {
      val sharedStateHandlersSet = sharedStateHandles.toSet

      { (context: Context, signal: Signal[_]) =>
        val oldState = context(subsystemStateHandle1).asInstanceOf[Context]
        val sharedStates = context.filterKeys(sharedStateHandlersSet.contains) // sharedStateHandles.map(ssh ⇒ (ssh, context(ssh)))
      val oldStateWithShared = oldState ++ sharedStates
        val (newChildContext: Context, signals) = proc(oldStateWithShared, signal)
        val (sharedStateValues, newStateWithoutShared) =
          newChildContext.toList.partition {
            case (sh, _) ⇒ sharedStateHandlersSet.contains(sh)
          }

        val resultingContext =
          (context updated(subsystemStateHandle1, newStateWithoutShared.toMap)) ++
            sharedStateValues
        (resultingContext, signals)
      }
    }

    /** Constructs a converter for inner systems. Two different cases are implemented. With shared
      * state handles and without them. In the latter case a simplier implementation is used. */
    def innerSystemToSignalProcessor(converterRecursive: ComponentDescriptorConverter,
                                     rsToTtp: RuntimeSystemToTotalTrellisProducerConverter):
    ComponentDescriptorConverter = {
      case ComponentDescriptor(InnerSystemComponent(subsystem, subsystemStateHandle, sharedStateHandles), path, _) ⇒
        val rs = systemToRuntimeSystem(subsystem.name :: path,
          subsystem,
          converterRecursive,
          subsystem.outputContacts)
        val proc = rsToTtp(rs) //rs.toTotalTrellisProducer
        if (sharedStateHandles.isEmpty) {
          RuntimeComponentStateFlatMap(subsystem.name,
            subsystem.inputs, subsystem.outputs, subsystemStateHandle, proc)
        } else {
          val stateHandles = subsystemStateHandle :: sharedStateHandles
          val procWithShared = innerSystemSignalHandlerWithShared(subsystemStateHandle, proc, sharedStateHandles)
          RuntimeComponentMultiState(subsystem.name, stateHandles, procWithShared)
        }
    }

    def redLinkToSignalProcessor(converterWithoutRedLinks: ComponentDescriptorConverter):
    ComponentDescriptorConverter = {
      case ComponentDescriptor(Link(from, to, label, RedMapLink(stopContacts)), path, system) ⇒
        val rs = systemToRuntimeSystem(path, system, converterWithoutRedLinks, stopContacts)
        val proc = rs.toTotalTrellisProducer // toRuntimeComponentHeavy(system.privateStateHandles)
        RuntimeComponentMultiState(system.name, system.privateStateHandles, (context, signal) ⇒ proc(context, Signal(to, signal.data)))
    }

    /** Ignores red links.
      * This converter is necessary to avoid infinite conversion of redlinks.
      */
    val redLinkDummy: ComponentDescriptorConverter = {
      case ComponentDescriptor(Link(from, to, label, RedMapLink(stopContacts)), path, system) ⇒
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


    /**
     * Constructs a complex function that converts components to a function that will do the work
     * when the data appears on one of the contacts.
     *
     * Create special construction for processing red links: on top it converts redlinks
     * as a special components. Inside the special red-link component redLinks are
     * converted to dummies. So there will be no infinite loop in conversion.
     *
     * Also the conterter has a proxy-trick for subsystems. The same converter
     * is used to convert high level systems and inner subsystems.
     */
    def componentToSignalProcessor2(
                                     rsToTtp: RuntimeSystemToTotalTrellisProducerConverter,
                                     simpleConverters: ComponentDescriptorConverter*): ComponentDescriptorConverter = {
      // the proxy will be filled with value afterwards. Thus we'll have a structural recursion.
      val combinedConverter = new ComponentDescriptorConverterProxy

      val inner = innerSystemToSignalProcessor(combinedConverter, rsToTtp)

      val converterWithoutRedLinks = new MutableComponentConverterBuilder
      converterWithoutRedLinks += redLinkDummy
      converterWithoutRedLinks ++= simpleConverters
      converterWithoutRedLinks += inner


      val converterWithRedLinks = new MutableComponentConverterBuilder
      converterWithRedLinks += redLinkToSignalProcessor(converterWithoutRedLinks.totalConverter)
      converterWithRedLinks ++= simpleConverters
      converterWithRedLinks += inner


      // here we finally fill-in the proxy's target value. And we now have a recursive structure.
      combinedConverter.target = converterWithRedLinks.totalConverter
      combinedConverter
    }


    // TODO: replace with dispatching red links
    def subsystemDirectProcessor(procs: Map[String, (RuntimeComponent, ContactsIndex)]): RuntimeComponent =
      RuntimeComponentMultiState("subsystemDirectProcessor", List(), {
        case (context, outerSignal) =>
          outerSignal match {
            case Signal(SubsystemSpecialContact, sig0) ⇒
              val sig1 = sig0.asInstanceOf[SubsystemDirectSignal0]
              val rt = procs.get(sig1.subsystemName)
              rt.map { rc =>
                val signal = sig0 match {
                  case SubsystemDirectSignal(_, s) => s
                  case SubsystemDirectSignalDist(_, d) => rc._2(d)
                }
                rc._1 match {
                  case RuntimeComponentMultiState(_, _, f) =>
                    f(context, signal)
                  case RuntimeComponentFlatMap(_, _, _, f) =>
                    val r = f(signal)
                    (context, r)
                  case RuntimeComponentStateFlatMap(_, _, _, sh, f) =>
                    val s = context(sh)
                    val r = f.asInstanceOf[(Any, Signal[_]) => (Any, List[Signal[_]])](s, signal)
                    (context.updated(sh, r._1), r._2)
                }
              }.
                getOrElse((context, List()))
            case Signal(SubsystemSpecialAnswerContact, sig0) ⇒
              debug("SubsystemSpecialAnswerContact: " + sig0)
              val sig1 = sig0.asInstanceOf[SubsystemDirectSignal0]
              val rt = procs.get(sig1.subsystemName)
              debug("SubsystemSpecialAnswerContact.rt: " + rt)
              val signals = rt.map { rc =>
                sig0 match {
                  case SubsystemDirectSignal(_, s) => s
                  case SubsystemDirectSignalDist(_, d) => rc._2(d)
                }
              }.toList
              debug("SubsystemSpecialAnswerContact.signals: " + signals)
              (context, signals)
            case _ =>
              throw new IllegalArgumentException(s"Wrong data on contact SubsystemSpecialContact: $outerSignal.")
          }
      })


    def systemToRuntimeSystem(path: List[String],
                              system: StaticSystem,
                              converter: ComponentDescriptorConverter,
                              stopContacts: Set[Contact[_]]): RuntimeSystem = {
      val processors = for {
        component ← system.components
        proc = converter(ComponentDescriptor(component, path, system)): RuntimeComponent
      } yield (component, proc)
      val contactsProcessors =
        for {
          (component, proc) ← processors
          i ← component.inputContacts
        } yield (i, proc): (Contact[_], RuntimeComponent)
      val innerSystems =
        processors.collect {
          case (comp: ComponentWithInternalStructure, proc) =>
            val s = comp.toStaticSystem
            (s.name, (proc, s.index))
        }
      val contactsProcessors2 =
        if (innerSystems.isEmpty)
          contactsProcessors
        else {
          val subsystemProc = subsystemDirectProcessor(innerSystems.toMap)
          (SubsystemSpecialContact, subsystemProc) ::
            (SubsystemSpecialAnswerContact, subsystemProc) ::
            contactsProcessors //(p => (SubsystemSpecialContact, p))
        }


      val lst = contactsProcessors2.groupBy(_._1).map(p ⇒ (p._1, p._2.map(_._2)))
      val signalProcessors = lst.toMap[Contact[_], List[RuntimeComponent]].withDefault(c ⇒ List())

      RuntimeSystem(system.name, signalProcessors, stopContacts, system.unhandledExceptionHandler)
    }

    def toRuntimeSystem(system: StaticSystem,
                        //inContacts: Set[Contact[_]],
                        stopContacts: Set[Contact[_]],
                        rsToTtp: RuntimeSystemToTotalTrellisProducerConverter): RuntimeSystem = {

      val m = componentToSignalProcessor2(rsToTtp, RuntimeComponent.linkToRuntimeComponent)

      systemToRuntimeSystem(List(), system, m, stopContacts)

    }

    def toSimpleSignalProcessor(path: List[String],
                                system: StaticSystem,
                                rsToTtp: RuntimeSystemToTotalTrellisProducerConverter): SimpleSignalProcessor = {
      /** The state of the system. */
      var state = system.s0

      val rs = systemToRuntimeSystem(path, system,
        componentToSignalProcessor2(rsToTtp, RuntimeComponent.linkToRuntimeComponent),
        system.outputContacts)
      val proc = rsToTtp(rs) // rs.toTotalTrellisProducer

      def receive(signal: Signal[_]): List[Signal[_]] = {
        def receive0(st: system.StateType, resSignals: List[TraversableOnce[Signal[_]]], signals: List[Signal[_]]): (system.StateType, List[TraversableOnce[Signal[_]]]) = signals match {
          case Nil ⇒ (st, resSignals)
          case head :: tail ⇒
            val (newState, newSignals) = proc(st, head)
            receive0(newState, newSignals :: resSignals, tail)
        }
        val (newState, results) = receive0(state, Nil, signal :: Nil)
        state = newState
        results.reverse.flatten
      }
      receive
    }

    def toDynamicSystem(path: List[String],
                        system: StaticSystem,
                        rsToTtp: RuntimeSystemToTotalTrellisProducerConverter) =
      new DynamicSystem(system.inputContacts, system.outputContacts, system.name,
        toSimpleSignalProcessor(path, system, rsToTtp), system.index)


  }

  implicit class RichRuntimeType[T](t: T)(implicit cvt: T => StaticSystem) {
    def toDynamicSystem = SystemConverting.toDynamicSystem(List(), t: StaticSystem, _.toTotalTrellisProducer)

    def toSimpleSignalProcessor = SystemConverting.toSimpleSignalProcessor(List(), t: StaticSystem, _.toTotalTrellisProducer)

    def toRuntimeSystem = SystemConverting.toRuntimeSystem(t: StaticSystem, (t: StaticSystem).outputContacts, _.toTotalTrellisProducer)


  }

  implicit class RichRuntimeStaticSystem(system: StaticSystem) {

    def toDynamicSystem = SystemConverting.toDynamicSystem(List(), system, _.toTotalTrellisProducer)

    def toSimpleSignalProcessor = SystemConverting.toSimpleSignalProcessor(List(), system, _.toTotalTrellisProducer)

    def toRuntimeSystem = SystemConverting.toRuntimeSystem(system, system.outputContacts, _.toTotalTrellisProducer)

    def allContacts = system.index.contacts


  }

}