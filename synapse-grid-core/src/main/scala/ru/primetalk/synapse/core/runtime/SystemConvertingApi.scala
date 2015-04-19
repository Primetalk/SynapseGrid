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

/**
 * For runtime processing StaticSystem is first converted (compiled) to RuntimeSystem that
 * is better suited for actual signals processing.
 *
 * All elements in StaticSystem are converted to RuntimeComponent. RuntimeComponent already contains
 * functions that process signals.
 *
 * From the system's converting point of view there are two kinds of StaticSystem Component's
 * simple and compex. Simple components do not depend on Subsystem's encapsulation.
 * Complex components do depend on subsystem's path. For simple components we may use simpler approach.
 *
 * Subsystems are converted with a special construction - RuntimeSystemToTotalTrellisProducerConverter.
 * This converter is a recursive converter of a StaticSystem because it is necessary to convert
 * both the top level system and all subsystems at any level.
 *
 *
 * --------------------
 *
 * Toolkit for conversion of StaticSystem to RuntimeSystems.
 * The conversion purpose is to convert all Components of a StaticSystem
 * to some kind of Signal processors.
 *
 * Partial functions are used as a basis for conversion algorithm composition. They have convenient pattern-matching
 * syntax and allow easy chaining.
 * However, construction of composite converters is a bit tricky:
 * - ensure recursive conversion of subsystems;
 * - ensure red links conversion with the whole system in mind.
 * - extensible to add new component types.
 *
 * It is possible to employ a slightly different approach. First get the class of a component and then
 * apply a partial function to the component. This approach however works only for simple one-level
 * components like links.
 */
trait SystemConvertingApi extends RuntimeComponentApi
with TrellisApi with RuntimeSystemApi with SignalProcessingDsl
with ContactsIndexExt with ExceptionHandlingExt {

  /** To enable debug information one may override #debug that is called within processing algorithms. */
  protected
  def debug(msg: => String): Unit = {
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

  /** Converter proxy is used to construct functional converter with
    * recursive internal structure (with data structure loop).
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


    //    class MutableComponentConverterBuilderOld extends MutableComponentConverterBuilder with ComponentDescriptorConverter {
    //      /** An alternative to `totalConverter` is to call MutableComponentConverterBuilder as a ComponentConverter. */
    //      def apply(tuple: ComponentDescriptor): RuntimeComponent = {
    //        val t = Try {
    //          totalConverter(tuple)
    //        }
    //        if (t.isSuccess)
    //          t.get
    //        else {
    //          val e = t.failed.get
    //          throw new RuntimeException(s"Cannot convert $tuple.", e)
    //        }
    //      }
    //
    //      def isDefinedAt(tuple: ComponentDescriptor) =
    //        totalConverter.isDefinedAt(tuple)
    //
    //    }

    val unmatched: ComponentDescriptorConverter = {
      case ComponentDescriptor(component, path, system) =>
        throw new IllegalArgumentException(
          s"The component $component cannot be converted to RuntimeComponent (path=$path, system=$system).")
    }

  }


  /** A lot of complex logic. */
  object SystemConverting {

    /** Constructs a trellis producer for a subsystem.
      * Inner states of the system are stored in a single state that is
      * addressed with subsystemStateHandle1. There are also shared states
      * that are addressed with sharedStateHandles.
      * For shared handles their state is retrieved from outer context before calling
      * `proc` and then returned back after `proc`.
      */
    private
    def innerSystemToSignalProcessor_HandlerWithShared(subsystemStateHandle1: Contact[Context],
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
          val procWithShared = innerSystemToSignalProcessor_HandlerWithShared(subsystemStateHandle, proc, sharedStateHandles)
          val stateHandles = subsystemStateHandle :: sharedStateHandles
          RuntimeComponentMultiState(subsystem.name, stateHandles, procWithShared)
        }
    }

    /** A converter of "red links" that creates a separate trellis producer instance that will
      * process signals in a single top level time step. */
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
     * Constructs a complex partial function that converts components to a function that will do the work
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

    def runtimeComponentToTotalTrellisProducer(rc: RuntimeComponent): TotalTrellisProducer =
      rc.toTotalTrellisProducer

//    def applyRCToSignal(rc: RuntimeComponent)(context: Context, signal: Signal[_]): (Context, SignalCollection[Signal[_]]) = rc match {
//      case RuntimeComponentMultiState(_, _, f) =>
//        f(context, signal)
//      case RuntimeComponentFlatMap(_, _, _, f) =>
//        (context, f(signal))
//      case RuntimeComponentStateFlatMap(_, _, _, sh, f) =>
//        val fun = f.asInstanceOf[(Any, Signal[_]) => (Any, SignalCollection[Signal[_]])]
//        val r = fun(context(sh), signal)
//        (context.updated(sh, r._1), r._2)
//    }

    /** RuntimeComponent that subscribes to SubsystemSpecialContact
      * and passes appropriate signals forward to the subsystem. */
    // TODO: replace with dispatching red links
    def subsystemSpecialContactDirectProcessor(procs: Map[String, (TotalTrellisProducer, ContactsIndex)]): RuntimeComponent = {
      RuntimeComponentMultiState("subsystemSpecialContactDirectProcessor", List(), {
        case (context, Signal(SubsystemSpecialContact, SubsystemDirectSignal(subsystemName, signal))) ⇒
          val rc = procs.getOrElse(subsystemName, throw new IllegalArgumentException(s"Cannot find subsystem $subsystemName"))
          rc._1(context, signal)
        case (context, Signal(SubsystemSpecialContact, SubsystemDirectSignalDist(subsystemName, d))) ⇒
          val rc = procs.getOrElse(subsystemName, throw new IllegalArgumentException(s"Cannot find subsystem $subsystemName"))
          rc._1(context, rc._2(d))
        case (_, outerSignal) =>
          throw new IllegalArgumentException(s"Wrong data on contact SubsystemSpecialContact: $outerSignal.")
      })
    }

    /** RuntimeComponent that subscribes to SubsystemSpecialAnswerContact
      * and passes appropriate signals backward to the system's level. */
    // TODO: replace with dispatching red links
    def subsystemSpecialAnswerContactDirectProcessor(procs: Map[String, (TotalTrellisProducer, ContactsIndex)]): RuntimeComponent = {
      RuntimeComponentMultiState("subsystemSpecialAnswerContactDirectProcessor", List(), {
        case (context, Signal(SubsystemSpecialAnswerContact, sig0@SubsystemDirectSignal(subsystemName, s))) ⇒
          (context, Iterable(s))
        case (context, Signal(SubsystemSpecialAnswerContact, sig0@SubsystemDirectSignalDist(subsystemName, d))) ⇒
          debug("SubsystemSpecialAnswerContact: " + sig0)
          val rt = procs.get(subsystemName)
          debug("SubsystemSpecialAnswerContact.rt: " + rt)
          val signals = rt.map { rc => rc._2(d) }.toIterable
          debug("SubsystemSpecialAnswerContact.signals: " + signals)
          (context, signals)
        case (_, outerSignal) =>
          throw new IllegalArgumentException(s"Wrong data on contact SubsystemSpecialAnswerContact: $outerSignal.")
      })
    }

    def systemToRuntimeSystem(path: List[String],
                              system: StaticSystem,
                              converter: ComponentDescriptorConverter,
                              stopContacts: Set[Contact[_]]): RuntimeSystem = {
      // converts all components to RuntimeComponent's
      val processors = for {
        component ← system.components
        rc = converter(ComponentDescriptor(component, path, system))
        proc = rc.toTotalTrellisProducer
      } yield (component, rc, proc)
      // collects all input contacts and associate processors with contacts
      val contactsProcessors = for {
        (component, rc, _) ← processors
        i ← component.inputContacts
      } yield (i, rc): (Contact[_], RuntimeComponent)
      // collects subsystems
      val subsystems = processors.collect {
        case (comp: ComponentWithInternalStructure, _, proc) =>
          val s = comp.toStaticSystem
          (s.name, (proc, s.index))
      }.toMap
      // add special processing for inner systems to be able to pass 
      val contactsProcessors2 =
        if (subsystems.isEmpty)
          contactsProcessors
        else {
          (SubsystemSpecialContact, subsystemSpecialContactDirectProcessor(subsystems)) ::
            (SubsystemSpecialAnswerContact, subsystemSpecialAnswerContactDirectProcessor(subsystems)) ::
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
      systemToRuntimeSystem(List(), system, componentToSignalProcessor2(rsToTtp, RuntimeComponent.linkToRuntimeComponent), stopContacts)
    }

    //    def convertToRuntimeSystem(system: StaticSystem,)
    def toSimpleSignalProcessor(path: List[String],
                                system: StaticSystem,
                                rsToTtp: RuntimeSystemToTotalTrellisProducerConverter): SimpleSignalProcessor = {

      val rs = systemToRuntimeSystem(path, system, componentToSignalProcessor2(rsToTtp, RuntimeComponent.linkToRuntimeComponent), system.outputContacts)
      val proc = rsToTtp(rs)
      proc.toSimpleSignalProcessor(system.s0)
    }

    def toDynamicSystem(path: List[String],
                        system: StaticSystem,
                        rsToTtp: RuntimeSystemToTotalTrellisProducerConverter) =
      new DynamicSystem(system.inputContacts, system.outputContacts, system.name,
        toSimpleSignalProcessor(path, system, rsToTtp), system.index)

  }

  /** Extension methods for any type convertible to StaticSystem.
    * Scalac substitutes identity function for T=StaticSystem.
    */
  implicit class RichRuntimeType[T](t: T)(implicit cvt: T => StaticSystem) {
    def toDynamicSystem = SystemConverting.toDynamicSystem(List(), t: StaticSystem, _.toTotalTrellisProducer)

    def toSimpleSignalProcessor = SystemConverting.toSimpleSignalProcessor(List(), t: StaticSystem, _.toTotalTrellisProducer)

    def toRuntimeSystem = SystemConverting.toRuntimeSystem(t: StaticSystem, (t: StaticSystem).outputContacts, _.toTotalTrellisProducer)

    def allContacts = (t: StaticSystem).index.contacts

  }

  //  implicit class RichRuntimeStaticSystem(system: StaticSystem) {
  //
  //    def toDynamicSystem = SystemConverting.toDynamicSystem(List(), system, _.toTotalTrellisProducer)
  //
  //    def toSimpleSignalProcessor = SystemConverting.toSimpleSignalProcessor(List(), system, _.toTotalTrellisProducer)
  //
  //    def toRuntimeSystem = SystemConverting.toRuntimeSystem(system, system.outputContacts, _.toTotalTrellisProducer)
  //
  //    def allContacts = system.index.contacts
  //
  //
  //  }

}