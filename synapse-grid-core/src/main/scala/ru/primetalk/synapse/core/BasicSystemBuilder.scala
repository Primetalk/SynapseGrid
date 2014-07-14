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
package ru.primetalk.synapse.core

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * This builder supports step-by-step creation of contact system. At the end
 * one must convert it to [[ru.primetalk.synapse.core.StaticSystem]].
 *
 * The builder supports the notion of extensions (much like akka actor system extensions).
 * When we need to store additional information during system construction,
 * we may request an extention instance from the builder. The builder creates
 * the singleton instance if it is not available yet.
 */
trait BasicSystemBuilder {
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


  private[core] val contacts = mutable.ListBuffer[Contact[_]]()
  private[core] val privateStateHandles = mutable.ListBuffer[StateHandle[_]]()
  private[core] val links = mutable.ListBuffer[Link[_, _, Nothing, Any]]()
  private[core] val components = mutable.ListBuffer[Component]()
  private[core] val inputContacts = mutable.Set[Contact[_]]()
  private[core] val outputContacts = mutable.Set[Contact[_]]()

  protected val extensions = mutable.Map[SystemBuilderExtensionId[_], Any]()

  /** Constructs the current version of static system. */
  def toStaticSystem = StaticSystem(
    /** A subset of contacts */
    inputContacts.toList.distinct,
    outputContacts.toList.distinct,
    privateStateHandles.toList,
    components.toList reverse_::: links.toList,
    name: String)

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
   * Add StateHandle to the system
   */
  def addStateHandle[S](sh: StateHandle[S]): StateHandle[S] = {
    assertWritable()
    privateStateHandles.
      find(sh0 => sh0.name == sh.name && sh0.s0 == sh.s0).
      getOrElse {
      privateStateHandles += sh; sh
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

  def addComponent(component: Component) {
    components += component
  }

  /**
   * Subsystem.
   * It can have a few input contacts (any number), however,
   * it will process signals by one.
   * If the subsystem has output contacts (it usually has), then the result of subsystem
   * processing will appear on the same contacts of the parent system.
   */
  def addSubsystem[T](system: T, sharedStateHandles: StateHandle[_]*)(implicit ev: T => StaticSystem): T = {
    val s = system: StaticSystem
    sharedStateHandles.foreach(addStateHandle(_))
    val s0withoutShared = s.s0 -- sharedStateHandles
    components += new InnerSystem(s, state(s.name + "State", s0withoutShared), sharedStateHandles.toList)
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

}
