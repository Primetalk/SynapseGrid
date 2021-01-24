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
package ru.primetalk.synapse.core.ext

import ru.primetalk.synapse.core.components.{Contact0, InnerSystemComponent, StateHandle0}
import ru.primetalk.synapse.core.dsl.ContactsDsl

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.implicitConversions

trait SystemBuilderApi extends ContactsDsl with ExceptionHandlingExt {

  /** An interface of some object that can collect information about outer interface of a system.
    * Not only it create contact instances, but it usually transforms and collect them. */
  trait OuterInterfaceBuilder {
    /** Changes the name of the component. */
    def setSystemName(name:String): Unit

    /** creates a contact that will be used as input contact. */
    def input[T](internalName: String): Contact[T]

    /** creates a contact that will be used as output contact. */
    def output[T](internalName: String): Contact[T]

  }

  /** DSL for constructing systems.
    *
    * This builder supports step-by-step creation of contact system. At the end
    * one must convert it to [[StaticSystem]].
    *
    * The builder supports the notion of extensions (much like akka actor system extensions).
    * When we need to store additional information during system construction,
    * we may request an extention instance from the builder. The builder creates
    * the singleton instance if it is not available yet and returns it.
    *
    * The builder is used in BaseTypedSystem as a DSL for system's construction.
    */
  trait SystemBuilder extends OuterInterfaceBuilder with WithStaticSystem {
    private var name =
      if getClass.isAnonymousClass then
        ""
      else
        getClass.getSimpleName.
          replace("Builder", "").
          replace("BuilderC", "").
          replace("$", "")

    def setSystemName(name: String): Unit = {
      this.name = name
    }

    def systemName: String = name

    def systemName_=(name: String): Unit = {
      this.name = name
    }


    //  private[core] val contacts = mutable.ListBuffer[Contact0]()
    private[core] val privateStateHandles = mutable.ListBuffer[StateHandle[_]]()
    private[core] val links = mutable.ListBuffer[Link[?, ?, ?, ?]]()
    private[core] val components = mutable.ListBuffer[Component]()
    private[core] val inputContacts = mutable.Set[Contact0]()
    private[core] val outputContacts = mutable.Set[Contact0]()
    private[core] var unhandledExceptionHandler = defaultUnhandledExceptionHandler

    private[core] val extensions = mutable.Map[SystemBuilderExtensionId[_], SystemBuilderExtension]()

    /** Constructs the current version of static system. */
    def toStaticSystem: StaticSystem = {
      val s0 = StaticSystem(
        /** A subset of contacts */
        inputContacts.toList.distinct,
        outputContacts.toList.distinct,
        privateStateHandles.toList,
        components.toList reverse_::: links.toList,
        name: String)
      val s1 = extensions.values.foldLeft(s0)((s, e) => e.postProcess(s))
      val s2 = if unhandledExceptionHandler != defaultUnhandledExceptionHandler then
        s1.extend(unhandledExceptionHandler)
      else
        s1
      s2
    }

    /**
     * Sets this builder to the read only mode. Subsequent modifications will lead to
     * exceptions.
     */
    def readOnly(): Unit = {
      isReadOnly = true
    }

    private var isReadOnly = false

    private[core] def assertWritable(): Unit = {
      if isReadOnly then
        throw new IllegalStateException(s"The system builder '$name' is in read only mode.")
    }

    /**
     * Makes the builder unmodifiable and returns completed static system.
     */
    def complete(): StaticSystem = {
      readOnly()
      toStaticSystem
    }


    /**
     * Create StateHandle and add it to the system
     */
    def state[S](name: String, initialValue: S): StateHandle[S] = {
      assertWritable()
      addStateHandle(StateHandle[S](name, initialValue))
    }

    def inputs(lc: Contact0*): Unit = {
      inputContacts ++= lc
    }

    def outputs(lc: Contact0*): Unit = {
      assertWritable()
      for c <- lc do {
        if links.exists(link => link.from == c) then
          throw new IllegalArgumentException(s"The contact $c cannot be added because there is a link such that link.from is this contact.")
        if components.exists(component => component.inputContacts.contains(c)) then
          throw new IllegalArgumentException(s"The contact $c cannot be added because there is a component such that component.inputContacts contains this contact.")

        outputContacts += c
      }
    }

    /**
     * Create contact and add it to the builder as an input
     */
    def input[T](name: String): Contact[T] = {
      val c = contact[T](name)
      inputs(c)
      c
    }

    /**
     * Create contact and add it to the builder as an output
     */
    def output[T](name: String): Contact[T] = {
      val c = contact[T](name)
      outputs(c)
      c
    }

    /**
     * Add StateHandle to the system
     */
    def addStateHandle[S](sh: StateHandle[S]): StateHandle[S] = {
      assertWritable()
      privateStateHandles
        .find(sh0 => sh0.name == sh.name && sh0.s0 == sh.s0)
        .getOrElse {
          privateStateHandles += sh
          sh
        }
        .asInstanceOf[StateHandle[S]]
      //    if(!privateStateHandles.contains(sh))
      //      privateStateHandles += sh
      //    sh
    }

    def addLink[T1, T2](from: Contact[T1], to: Contact[T2], name: String, info: LinkInfo[T1, T2]): Contact[T2] = {
      val link = Link(from, to, name, info)
      addLink(link)
    }

    /**
      */
    def addLink[T1, T2](link: Link[T1, T2, T1, T2]): Contact[T2] = {
      assertWritable()
      if outputContacts.contains(link.from) then
        throw new IllegalArgumentException(s"The link $link cannot be added because ${link.from.name} is output contact.")
      links += link
      link.to
    }

    /** Adds a self contained component.
      * NB: The state of the component is not managed! */
    def addComponent[T <: Component](component: T): T = {
      components += component
      component
    }

    given Conversion[StaticSystem, StaticSystem] = identity
    /**
     * Subsystem.
     * It can have a few input contacts (any number), however,
     * it will process signals by one.
     *
     * If the subsystem has output contacts (it usually has), then the result of subsystem
     * processing will appear on the same contacts of the parent system.
     *
     * @param sharedStateHandles a collection of state handles that is shared between the parent
     *                           and the child system. Whenever the system gets a signal, shared
     *                           state values are copied into it's internal state.
     */
    def addSubsystem[T](system: T, sharedStateHandles: StateHandle0*)(using Conversion[T, StaticSystem]): T = {
      val s: StaticSystem = system
      sharedStateHandles.foreach(sh0 =>addStateHandle(sh0.asInstanceOf[StateHandle[_]]))
      val s0withoutShared = s.s0 -- sharedStateHandles
      components += new InnerSystemComponent(s, state(s.name + "State", s0withoutShared), sharedStateHandles.toList)
      system
    }

    /** Creates subsystem and adds it to this system as a component.
      *
      * @param name the name of the component
      * @param outerBuilder function that creates outer interface of the system of type T
      * @param definitionBuilder function that constructs StaticSystem for type T.
      * @tparam T outer system's interface type
      * @return outer system's interface
      */
    def newSubsystem[T](name:String)(implicit outerBuilder:OuterInterfaceBuilder => T, definitionBuilder: T => StaticSystem):T = {
      val sb1 = new SystemBuilderC(name)
      val s = outerBuilder(sb1)
      val staticSystem = definitionBuilder(s)
      s
    }
    /** Adds a few subsystems at once. Useful for super systems construction. */
    def addSubsystems(subsystems: StaticSystem*): Unit =
      subsystems.foreach(subsystem => addSubsystem(subsystem))

    // Static analysis of the system's graph

    /** returns one step successors from the given contact */
    def successors(c: Contact0): List[Contact0] = {
      val linkSuccessors = links.toList.filter(_.from == c).map(_.to)
      val compSuccessors = components.toList.filter(_.inputContacts.contains(c)).flatMap(_.outputContacts)
      (linkSuccessors ++ compSuccessors).distinct
    }

    /** returns one step successors from the given contact */
    def predecessors(c: Contact0): List[Contact0] = {
      val linkPredecessors = links.toList.filter(_.to == c).map(_.from)
      val compPredecessors = components.toList.filter(_.outputContacts.contains(c)).flatMap(_.inputContacts)
      (linkPredecessors ++ compPredecessors).distinct
    }

    /** Calculates the number of transitions from c1 to that contact. If the contact is not reachable
      * then the distance is equals = -1 */
    def minDistance(c1: Contact0, c2: Contact0): Int = {
      @tailrec
      def minDistance(toCheck: Set[Contact0], exclude: Set[Contact0], dist: Int = 0): Int = {
        if toCheck.isEmpty then
          -1
        else if toCheck.contains(c2) then
          dist
        else
          minDistance((toCheck flatMap successors) -- exclude, exclude ++ toCheck, dist + 1)
      }
      minDistance(Set(c1), Set())
    }

    def extend[T <: SystemBuilderExtension](implicit extensionId: SystemBuilderExtensionId[T]): T =
      extensions.
        getOrElseUpdate(extensionId,
          extensionId.extend(this)).
        asInstanceOf[T]

    def handleExceptions(handler: UnhandledProcessingExceptionHandler): Unit = {
      unhandledExceptionHandler = handler
    }

    def findInput(name: String): Option[Contact0] = inputContacts.find(_.name == name)

    def findOutput(name: String): Option[Contact0] = outputContacts.find(_.name == name)
  }

  /** An extension that adds some additional state to SystemBuilder.
    * It also has an opportunity to adjust the generated StaticSystem via hook #postProcess
    */
  trait SystemBuilderExtension {
    // the extended SystemBuilder
    val sb: SystemBuilder

    /** Opportunity for extension to hook into method
      * SystemBuilder#toStaticSystem".
      * It can also add some information to extensions map. */
    def postProcess(s: StaticSystem): StaticSystem = s
  }

  /** ExtensionId for a system builder. Every extension can be
    * installed only once on the same SystemBuilder. An instance of the extension
    * is created by the extend method.
    * @param extend 	This method is called once for a system builder. No need to check.
    */
  final class SystemBuilderExtensionId[T <: SystemBuilderExtension](val extend: SystemBuilder => T)

  class SystemBuilderC(name: String = "") extends SystemBuilder {
    implicit def sb: SystemBuilder = this
    if name!="" then
      this.setSystemName(name)
  }

  def state[T](name:String, s0:T)(implicit sb:SystemBuilder): StateHandle[T] = sb.state(name, s0)

  def setSystemName(name:String)(implicit sb:SystemBuilder): Unit = sb.setSystemName(name)

  /** Usage:
    * extension[LabellingExt].methodInExtension*/
  def defineExtension[T <: SystemBuilderExtension](implicit sb: SystemBuilder,
                                                   extensionInstance: SystemBuilderExtensionId[T]): T =
    sb.extend(extensionInstance)

  /** Automatic usage of extensions when an implicit extension id is present in the scope.*/
  implicit def implicitExtendBasicSystemBuilder[T <: SystemBuilderExtension](sb: SystemBuilder)(
    implicit extensionInstanceId: SystemBuilderExtensionId[T]): T =
    sb.extend(extensionInstanceId)

//  trait WithStaticSystem {
//    def toStaticSystem: StaticSystem
//  }
//
//  implicit def withStaticSystemToStaticSystem(ws:WithStaticSystem): StaticSystem = ws.toStaticSystem
}