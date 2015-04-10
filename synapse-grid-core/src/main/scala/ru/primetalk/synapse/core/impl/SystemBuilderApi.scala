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
package ru.primetalk.synapse.core.impl

import ru.primetalk.synapse.core.{StateHandle, Contact}
import ru.primetalk.synapse.core.components._

import scala.annotation.tailrec
import scala.collection.mutable

trait SystemBuilderApi extends ContactsApi with ExceptionHandlingApi {

  /** An interface of some object that can collect information about outer interface of a system.
    * Not only it create contact instances, but it usually transforms and collect them. */
  trait OuterInterfaceBuilder {
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
  trait SystemBuilder extends OuterInterfaceBuilder {
    private var name =
      if (getClass.isAnonymousClass)
        ""
      else
        getClass.getSimpleName.
          replaceAllLiterally("Builder", "").
          replaceAllLiterally("BuilderC", "").
          replaceAllLiterally("$", "")

    def setSystemName(name: String) {
      this.name = name
    }

    def systemName = name

    def systemName_=(name: String) {
      this.name = name
    }


    //  private[core] val contacts = mutable.ListBuffer[Contact[_]]()
    private[core] val privateStateHandles = mutable.ListBuffer[StateHandle[_]]()
    private[core] val links = mutable.ListBuffer[Link[_, _, Nothing, Any]]()
    private[core] val components = mutable.ListBuffer[Component]()
    private[core] val inputContacts = mutable.Set[Contact[_]]()
    private[core] val outputContacts = mutable.Set[Contact[_]]()
    private[core] var unhandledExceptionHandler = defaultUnhandledExceptionHandler

    private[core] val extensions = mutable.Map[SystemBuilderExtensionId[_], SystemBuilderExtension]()

    /** Constructs the current version of static system. */
    def toStaticSystem = {
      val s0 = StaticSystem(
        /** A subset of contacts */
        inputContacts.toList.distinct,
        outputContacts.toList.distinct,
        privateStateHandles.toList,
        components.toList reverse_::: links.toList,
        name: String)
      val s1 = extensions.values.foldLeft(s0)((s, e) => e.postProcess(s))
      val s2 = if(unhandledExceptionHandler != defaultUnhandledExceptionHandler)
        s1.extend(unhandledExceptionHandler)
      else
        s1
      s2
    }

    /**
     * Sets this builder to the read only mode. Subsequent modifications will lead to
     * exceptions.
     */
    def readOnly() {
      isReadOnly = true
    }

    private var isReadOnly = false

    private[core] def assertWritable() {
      if (isReadOnly)
        throw new IllegalStateException(s"The system builder '$name' is in read only mode.")
    }

    /**
     * Makes the builder unmodifiable and returns completed static system.
     */
    def complete() = {
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

    def inputs(lc: Contact[_]*) {
      inputContacts ++= lc
    }

    def outputs(lc: Contact[_]*) {
      assertWritable()
      for (c <- lc) {
        if (links.exists(link => link.from == c))
          throw new IllegalArgumentException(s"The contact $c cannot be added because there is a link such that link.from is this contact.")
        if (components.exists(component => component.inputContacts.contains(c)))
          throw new IllegalArgumentException(s"The contact $c cannot be added because there is a component such that component.inputContacts contains this contact.")

        outputContacts += c
      }
    }

    /**
     * Create contact and add it to the builder as an input
     */
    def input[T](name: String) = {
      val c = contact[T](name)
      inputs(c)
      c
    }

    /**
     * Create contact and add it to the builder as an output
     */
    def output[T](name: String) = {
      val c = contact[T](name)
      outputs(c)
      c
    }

    /**
     * Add StateHandle to the system
     */
    def addStateHandle[S](sh: StateHandle[S]): StateHandle[S] = {
      assertWritable()
      privateStateHandles.
        find(sh0 => sh0.name == sh.name && sh0.s0 == sh.s0).
        getOrElse {
        privateStateHandles += sh
        sh
      }.
        asInstanceOf[StateHandle[S]]
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
      if (outputContacts.contains(link.from))
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
    def addSubsystem[T](system: T, sharedStateHandles: StateHandle[_]*)(implicit ev: T => StaticSystem): T = {
      val s = system: StaticSystem
      sharedStateHandles.foreach(addStateHandle(_))
      val s0withoutShared = s.s0 -- sharedStateHandles
      components += new InnerSystemComponent(s, state(s.name + "State", s0withoutShared), sharedStateHandles.toList)
      system
    }

    /** Adds a few subsystems at once. Useful for super systems construction. */
    def addSubsystems(subsystems: StaticSystem*) {
      subsystems.foreach(subsystem => addSubsystem(subsystem)(identity))
    }


    // Static analysis of the system's graph

    /** returns one step successors from the given contact */
    def successors(c: Contact[_]): List[Contact[_]] = {
      val linkSuccessors = links.toList.filter(_.from == c).map(_.to)
      val compSuccessors = components.toList.filter(_.inputContacts.contains(c)).flatMap(_.outputContacts)
      (linkSuccessors ++ compSuccessors).distinct
    }

    /** returns one step successors from the given contact */
    def predecessors(c: Contact[_]): List[Contact[_]] = {
      val linkPredecessors = links.toList.filter(_.to == c).map(_.from)
      val compPredecessors = components.toList.filter(_.outputContacts.contains(c)).flatMap(_.inputContacts)
      (linkPredecessors ++ compPredecessors).distinct
    }

    /** Calculates the number of transitions from c1 to that contact. If the contact is not reachable
      * then the distance is equals = -1 */
    def minDistance(c1: Contact[_], c2: Contact[_]) = {
      @tailrec
      def minDistance(toCheck: Set[Contact[_]], exclude: Set[Contact[_]], dist: Int = 0): Int = {
        if (toCheck.isEmpty)
          -1
        else if (toCheck.contains(c2))
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

    def findInput(name: String): Option[Contact[_]] = inputContacts.find(_.name == name)

    def findOutput(name: String): Option[Contact[_]] = outputContacts.find(_.name == name)
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

  class SystemBuilderC(name: String) extends SystemBuilder {
    implicit def sb: SystemBuilder = this
    this.setSystemName(name)
  }

  def state[T](name:String, s0:T)(implicit sb:SystemBuilder) = sb.state(name, s0)

  def setSystemName(name:String)(implicit sb:SystemBuilder) = sb.setSystemName(name)

}