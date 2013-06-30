///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SinapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 * (2-clause BSD license) See LICENSE
 *
 * Created: 28.06.13, zhizhelev
 */
package ru.primetalk.sinapse.core

import scala.annotation.tailrec

/**
 * This builder supports step-by-step creation of contact system. At the end
 * one must convert it to [[StaticSystem]].
 */
trait BasicSystemBuilder {
  private var name = getClass.getSimpleName.
    replaceAllLiterally("Builder", "").
    replaceAllLiterally("BuilderC", "").
    replaceAllLiterally("$", "_")

  def setSystemName(name : String) {
    this.name = name
  }

  import scala.collection.mutable

  protected val contacts = mutable.ListBuffer[Contact[_]]()
  protected val privateStateHandles = mutable.ListBuffer[StateHandle[_]]()
  protected val links = mutable.ListBuffer[Link[_, _, Nothing, Any]]()
  protected val components = mutable.ListBuffer[OuterSystem]()
  protected val inputContacts = mutable.Set[Contact[_]]()
  protected val outputContacts = mutable.Set[Contact[_]]()

  /** Constructs new version of static system. */
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

  protected def assertWritable() {
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
  def state[S](name : String, initialValue : S) : StateHandle[S] = {
    assertWritable()
    addStateHandle( StateHandle[S](name, initialValue))
  }
  /**
   * Add StateHandle to the system
   */
  def addStateHandle[S](sh : StateHandle[S]) : StateHandle[S] = {
    assertWritable()
    if(!privateStateHandles.contains(sh))
      privateStateHandles += sh
    sh
  }
  def addLink[T1, T2](from:Contact[T1], to:Contact[T2], info: LinkInfo[T1, T2]):Contact[T2] = {
    val link = Link(from, to, info)
    addLink(link)
  }
  /**
    */
  def addLink[T1, T2](link : Link[T1, T2, T1, T2]):Contact[T2] = {
    assertWritable()
    if(outputContacts.contains(link.from))
      throw new IllegalArgumentException(s"The link $link cannot be added because ${link.from.name} is output contact.")
    links += link
    link.to
  }
  def addComponent(component:OuterSystem){
    components += component
  }
  /**
   * Subsystem.
   * It can have a few input contacts (any number), however,
   * it will process signals by one.
   * If the subsystem has output contacts (it usually has), then the result of subsystem
   * processing will appear on the same contacts of the parent system.
   */
  def addSubsystem(s:StaticSystem, sharedStateHandles: StateHandle[_]*)= {
    sharedStateHandles.foreach(addStateHandle(_))
    val s0withoutShared = s.s0 -- sharedStateHandles
    components += new InnerSystem(s, state(s.name+"State", s0withoutShared), sharedStateHandles.toList)
  }

  // Static analysis of the system's graph

  /** returns one step successors from the given contact */
  def successors(c:Contact[_]):List[Contact[_]] = {
    val linkSuccessors = links.toList.filter(_.from == c).map(_.to)
    val compSuccessors = components.toList.filter(_.inputContacts.contains(c)).flatMap(_.outputContacts)
    (linkSuccessors ++ compSuccessors).distinct
  }
  /** Calculates the number of transitions from c1 to that contact. If the contact is not reachable
    *  then the distance is equals = -1*/
  def minDistance(c1 : Contact[_], c2 : Contact[_]) = {
    @tailrec
    def minDistance(toCheck:Set[Contact[_]], exclude:Set[Contact[_]], dist:Int = 0):Int = {
      if(toCheck.isEmpty)
        -1
      else if(toCheck.contains(c2))
        dist
      else
        minDistance((toCheck flatMap successors) -- exclude, exclude ++ toCheck, dist + 1)
    }
    minDistance(Set(c1), Set())
  }

}
