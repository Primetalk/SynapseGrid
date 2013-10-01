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

import scala.reflect.ClassTag
import scala.collection.{GenTraversableOnce, mutable}

/** DSL for constructing systems */
trait SystemBuilder extends BasicSystemBuilder {
  // TODO: сделать макросы вида: state counterS:Int = 0 contact myContact:String

  private var auxContactNumber = 0

  def nextContactName = {
    assertWritable()
    auxContactNumber += 1
    "c" + (auxContactNumber - 1)
  }

  private[synapse] var proposedLabels = List[String]()

  /**
   * Defines the sequence of labels to be used for superscription of links.
   */
  def labels(labels: String*) = {
    assertWritable()
    proposedLabels = labels.toList ::: proposedLabels
    this
  }

  private[synapse] def nextLabel(userProvidedLabel: String, defaultLabel: => String): String =
    (userProvidedLabel, proposedLabels) match {
      case ("", List()) ⇒ defaultLabel
      case ("", head :: tail) ⇒
        assertWritable()
        proposedLabels = tail
        head
      case (label, _) => label
    }

  /**
   * Create contact and add it to the builder
   */
  def contact[T](name: String) =
    new Contact[T](name)

  def auxContact[T] =
    new Contact[T](nextContactName, AuxiliaryContact)

  /**
   * Create contact and add it to the builder
   */
  def input[T](name: String) = {
    val c = contact[T](name)
    inputs(c)
    c
  }
  /**
   * Create contact and add it to the builder
   */
  def output[T](name: String) = {
    val c = contact[T](name)
    outputs(c)
    c
  }


  def connect[T1, T2 >:T1](c1:Contact[T1], c2:Contact[T2]) {
    c1 >> c2
  }

  /**
   * Special contact for consuming unnecessary data values.
   */
  lazy val devNull = new Contact[Any]("devNull", DevNullContact)

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

  /** Declares the first contact as input and creates link to the second*/
  def mappedInput[T, T2 >: T](c1:Contact[T], c2:Contact[T2]) = {
    inputs(c1)
    c1 >> c2
    c2
  }
  /** Declares the second contact as output and creates link from the first*/
  def mappedOutput[T, T2 >: T](c1:Contact[T], c2:Contact[T2]){
    outputs(c2)
    c1 >> c2
  }
  /**
   * Doesn't work because T2 is unknown when it is called implicitly.
   * <pre>
   * implicit def contactToLink[T1, T2](c1:Contact[T1]) = {
   * val c2 = addContact(new Contact[T2](nextContactName, AuxiliaryContact))
   * new ImplLinkBuilder(c1, c2)
   * }
   * </pre>
   */
  implicit class ImplDirectLinkBuilder[T1, T2 >: T1](p: (Contact[T1], Contact[T2])) {
    def directly(name: String = "Δt") =
      addLink(p._1, p._2, new NopLink[T1, T2](name))

    def filter(predicate: T1 ⇒ Boolean, name: String = "") = //: FlatMapLink[T1, T2, Seq[T2]] =
      addLink(p._1, p._2,
        new FlatMapLink[T1, T2]({
          x ⇒
            if (predicate(x))
              Seq(x: T2)
            else
              Seq[T2]()
        }, nextLabel(name, if (name.endsWith("?")) name else name + "?"))) //.asInstanceOf[FlatMapLink[T1, T2, Seq[T2]]] //[T1, T2, MapLink[T1,T2]]
  }
  implicit class ImplRichContactPair[S, T](c : Contact[(S, T)]) {//extends ImplRichContact[(S, T)](c) {
    require(c!= null, "Contact is null")

    /** Converts data to pair with current state value. */
    def unzipWithState(stateHandle : StateHandle[S], name:String = "") : Contact[T] = {
      (c -> auxContact[T]) .stateMap(stateHandle, nextLabel(name, "unzip to "+stateHandle)) ((s, p:(S, T)) ⇒ (p._1, p._2))
//      (new LinkWithState(c, auxContact[T], stateHandle)).stateMap((s, p) ⇒ (p._1, p._2), nextLabel(name, "unzip to "+stateHolder))
    }
    /** Switches based on the first element of the pair.*/
    def Case(CaseValue:S):Contact[T] = {
      c -> auxContact[T] collect ({ case (CaseValue, value) => value },s"Case($CaseValue)")
    }
    //		/** Switches based on the first element of the pair.*/
    //		def Case2[T2<:T : ClassTag](CaseValue:S,c2:Contact[T2]):Contact[T2] = {
    //			c -> c2 collect { case (CaseValue, value) if classOf[T2].runtimeClass.isInstance(value) => value .asInstanceOf[T2]}
    //		}
  }
  implicit class ImplLinkBuilder[T1, T2](c: (Contact[T1], Contact[T2])) {
    def labelNext(label: String*) = {
      labels(label: _*)
      this
    }

    def map(f: T1 ⇒ T2, name: String = ""): Contact[T2] =
      addLink(c._1, c._2, new FlatMapLink[T1, T2](x=>Seq(f(x)), nextLabel(name, "" + f)))

    def const(value: T2, name: String = ""): Contact[T2] =
      addLink(c._1, c._2, new FlatMapLink[T1, T2]((t: T1) => Seq(value), nextLabel(name, "⇒" + value)))

    def flatMap[TSeq](f: T1 ⇒ GenTraversableOnce[T2], name: String = "") =
      addLink(c._1, c._2, new FlatMapLink[T1, T2](f, nextLabel(name, "" + f)))

    def optionalMap(f: T1 ⇒ Option[T2], name: String = "") = //: FlatMapLink[T1, T2, Seq[T2]] =
      addLink(c._1, c._2, new FlatMapLink[T1, T2](f(_).toSeq, nextLabel(name, "" + f))) //.asInstanceOf[FlatMapLink[T1, T2, TSeq]] //[T1, T2, MapLink[T1,T2]]
    /** Cast data to the given class if possible */
    def castFilter[T3 <: T2](t2Class: Class[T3], name: String = "") = {
      addLink(c._1, c._2,
        new FlatMapLink[T1, T2](
          d ⇒ if (t2Class.isInstance(d))
            Seq(d.asInstanceOf[T2])
          else
            Seq(),
          nextLabel(name, "cast(" + t2Class.getSimpleName + ")")))
    }

    def castFilter2[T3 <: T2](implicit t3Class: ClassTag[T3]) = {
      addLink(c._1, c._2,
        new FlatMapLink[T1, T2]({
          case t3Class(d) => Seq(d.asInstanceOf[T2])
          case _ => Seq()
        },
        nextLabel("", "cast2(" + t3Class.runtimeClass.getSimpleName + ")")))
    }

    def collect(f: PartialFunction[T1, T2], name: String = "") =
      flatMap(t => {
        if (f.isDefinedAt(t)) Seq(f(t)) else Seq()
      }, name)

  }

  implicit class ImplStateLinkBuilder[T1, T2](c: (Contact[T1], Contact[T2])) {
    def stateMap[S](stateHandle: StateHandle[S], name: String = "")
                   (f: (S, T1) ⇒ (S, T2)) =
      addLink(c._1, c._2,
        new StatefulFlatMapLink[S, T1, T2, GenTraversableOnce[T2]](
        (s, t) => {val r = f(s,t);(r._1, Seq(r._2))}, stateHandle,
        nextLabel(name, "sm")))
    def stateFlatMap[S](stateHandle: StateHandle[S], name: String = "")(f: (S, T1) ⇒ (S, GenTraversableOnce[T2])) =
      addLink(c._1, c._2, new StatefulFlatMapLink[S, T1, T2, GenTraversableOnce[T2]](f, stateHandle, nextLabel(name, "sfm")))

  }

  class ContactWithState[T1, S](val c1:Contact[T1], val stateHandle:StateHandle[S]) {
    def stateMap[T2](f: (S, T1) ⇒ (S, T2), name:String ="") =
      addLink(c1, auxContact[T2],
        new StatefulFlatMapLink[S, T1, T2, GenTraversableOnce[T2]](
          (s, t) => {val r = f(s,t);(r._1, Seq(r._2))}, stateHandle,
        nextLabel(name, "sm")))
    def stateFlatMap[T2, TSeq <: GenTraversableOnce[T2]](f: (S, T1) ⇒ (S, TSeq), name:String ="") =
      addLink(c1, auxContact[T2], new StatefulFlatMapLink[S,T1, T2, TSeq](f, stateHandle,
        nextLabel(name, "sfm")))

    def updateState(name: String = "")(fun: (S, T1) ⇒ S) {
      addComponent(new StateUpdate[S, T1](c1, stateHandle, nextLabel(name, "update(" + fun + "," + stateHandle + ")"), fun))
    }

  }

  implicit class ImplStateLinkBuilder2[T1, T2, S](p:(ContactWithState[T1, S], Contact[T2])) {
    def stateMap(f: (S, T1) ⇒ (S, T2), name:String ="") =
      addLink(p._1.c1, p._2,
        new StatefulFlatMapLink[S, T1, T2, GenTraversableOnce[T2]](
          (s, t) => {val r = f(s,t);(r._1, Seq(r._2))}, p._1.stateHandle,
        nextLabel(name, "sm")))
    def stateFlatMap[TSeq <: GenTraversableOnce[T2]](f: (S, T1) ⇒ (S, TSeq), name:String ="") =
      addLink(p._1.c1, p._2, new StatefulFlatMapLink[S,T1, T2, TSeq](f, p._1.stateHandle,
        nextLabel(name, "sfm")))
  }
  implicit class ImplRichContact[T](val c: Contact[T]) {
    require(c != null, "Contact is null")

    def labelNext(label: String*) = {
      labels(label: _*)
      c
    }

    def output() {
      outputs(c)
    }

    def input: Contact[T] = {
      inputs(c)
      c
    }
    /** Declares the first contact as input and creates link to the second*/
    def inputMappedTo[T2 >: T](c2:Contact[T2]) = {
      inputs(c)
      c >> c2
      c2
    }
    /** Declares the second contact as output and creates link from the first*/
    def mapToOutput[T2 >: T](c2:Contact[T2]){
      outputs(c2)
      c >> c2
    }

    def stock(f: T ⇒ Any, name: String = "") {
      addLink(c, devNull, new FlatMapLink[T, Any](x=>Seq(f(x)), nextLabel(name, ">>null")))

    }


    /**
     * Filters the data from this contact. Returns another contact that will get filtered data
     */
    def filter(predicate: T ⇒ Boolean, name: String = ""): Contact[T] =
      new ImplDirectLinkBuilder[T, T](c, auxContact).filter(predicate, nextLabel(name, "" + predicate + "?"))

    /**
     * Filters the data from this contact. Returns another contact that will get filtered data
     */
    def withFilter(predicate: T ⇒ Boolean): Contact[T] =
      filter(predicate)



    def mapTo[T2](f: T ⇒ T2, auxContact1: Contact[T2] = auxContact[T2]): Contact[T2] =
    //			val auxContact = addContact(new Contact[T2](nextContactName, AuxiliaryContact))
      new ImplLinkBuilder(c, auxContact1).map(f, nextLabel("", "mapTo(" + f + ")"))

    def map[T2](f: T ⇒ T2, name: String = ""): Contact[T2] =
      new ImplLinkBuilder(c, auxContact[T2]).map(f, nextLabel(name, "map(" + f + ")"))

    def const[T2](value: T2, name: String = ""): Contact[T2] =
      addLink(c, auxContact[T2], new FlatMapLink[T, T2]((t: T) => Seq(value), nextLabel(name, "⇒" + value)))

    def castFilter2[T3](implicit t3Class: ClassTag[T3]) =
      new ImplLinkBuilder(c, auxContact[T3]).castFilter(t3Class.runtimeClass.asInstanceOf[Class[T3]])

    def castFilter[T3](t3Class: Class[T3], name: String = "") =
      new ImplLinkBuilder(c, auxContact[T3]).castFilter(t3Class)

    def collect[T2](f: PartialFunction[T, T2], name: String = "") =
      new ImplLinkBuilder(c, auxContact[T2]).flatMap(t => {
        if (f.isDefinedAt(t)) Seq(f(t)) else Seq()
      }, name)

    def passByStateCondition[S](stateHandle : StateHandle[S], name: String = "")(condition:S=>Boolean):Contact[T] = {
      val res = auxContact[T]
      (c -> res).stateFlatMap(stateHandle, nextLabel(name, "pass if condition on "+stateHandle.name))( (s,t) => if(condition(s))(s,Seq(t))else(s,Seq()) )
      res
    }
    def passIfEnabled(stateHandle : StateHandle[Boolean], name: String = "") =
      passByStateCondition(stateHandle, nextLabel(name, "pass if "+stateHandle.name+"?")) (identity)
//      new LinkWithState(c, auxContact[T], stateHolder).
//        stateFlatMap(
//        (flag, v) ⇒
//          if (flag)
//            (flag, Seq(v))
//          else
//            (flag, Seq()),
//        "pass if "+stateHolder.name+"?")
    def activate(stateHolder: StateHandle[Boolean], isActive: Boolean = true) {
      labelNext("⇒" + isActive).map(_ ⇒ isActive).saveTo(stateHolder)
    }

    def deactivate(stateHolder: StateHandle[Boolean]) {
      activate(stateHolder, isActive = false)
    }

    def flatMap[T2](f: T ⇒ TraversableOnce[T2], name: String = ""): Contact[T2] =
      new ImplLinkBuilder(c, auxContact[T2]).flatMap(f, nextLabel(name, "fM(" + f + ")"))

    def splitToElements[T2](name: String = "")(implicit ev: T <:< TraversableOnce[T2]): Contact[T2] =
      flatMap(t => ev(t), nextLabel(name, "split"))

    def foreach(body: T ⇒ Any, name: String = "") = {
      addLink(c, devNull, new FlatMapLink[T, Any](x=>{body(x);Seq()}, nextLabel(name, "foreach")))
      c
    }

    def exec(body: ⇒ Any, name: String = "") = {
      addLink(c, auxContact[T], new FlatMapLink[T, T]((t: T) => {
        body; Seq(t)
      }, nextLabel(name, "exec")))
    }

    /** Update state in state handle. */

    def updateState[S](stateHandle: StateHandle[S], name: String = "")(fun: (S, T) ⇒ S) {
      addComponent(new StateUpdate[S, T](c, stateHandle, nextLabel(name, "update(" + fun + "," + stateHandle + ")"), fun))
    }

    def inc[S](stateHandle: StateHandle[S], name: String = "")(implicit ev: S <:< Numeric[S], n : Numeric[S]) {
      addComponent(new StateUpdate[S, T](c, stateHandle,
        nextLabel(name, "inc(" + stateHandle + ")"), (s, _) => n.plus(s , n.one)))
    }
    def withState[S](stateHandle: StateHandle[S]) = new ContactWithState[T, S](c, stateHandle)

    def saveTo[S >: T](stateHolder: StateHandle[S], name: String = "") {
      addComponent(new StateUpdate[S, T](c, stateHolder, nextLabel(name, "saveTo(" + stateHolder + ")"), (s, t) ⇒ t))
    }

    def prependList[S >: T](stateHolder: StateHandle[List[S]], name: String = "") {
      updateState(stateHolder, nextLabel(name, "addToList(" + stateHolder + ")"))((list, item) ⇒ item :: list)
    }

    def clearList[S](stateHolder: StateHandle[List[S]], name: String = "") {
      updateState(stateHolder, nextLabel(name, "clearList(" + stateHolder + ")"))((list, item) ⇒ Nil)
    }

    def setState[S](stateHandle: StateHandle[S], name: String = "")(fun: T ⇒ S) = {
      new ImplStateLinkBuilder(c, devNull).stateFlatMap(
        stateHandle, nextLabel(name, "" + stateHandle + " := setState(" + fun + ")"))
      {(s: S, t: T) ⇒ (fun(t), Seq())}
    }

    /**
     * Latch is a state of type Option[S]. It can be cleared to None by one signal,
     * and set to Some() by another signal. After setting it doesn't change until cleared.
     */
    def clearLatch[S](stateHolder: StateHandle[Option[S]]) {
      labelNext("⇒None").map(any ⇒ None).saveTo(stateHolder)
    }

    /** Sets latch value it it was not set yet */
    def latchValue[S >: T](stateHolder: StateHandle[Option[S]], f: T ⇒ S = identity[T](_)) = {
      new ImplStateLinkBuilder(c, devNull).stateFlatMap(stateHolder, nextLabel("", "" + stateHolder + "<?=Some"))
      {(s: Option[S], t: T) ⇒ (if (s.isEmpty) Some(f(t)) else s, Seq())}

    }

    /** Converts data to pair with current state value. */
    def zipWithConst[T2](value: T2, name: String = ""): Contact[(T2, T)] =
      map((value, _), nextLabel(name, s"($value, _)"))

    /** Extracts current state value. */
    def getState[S](stateHolder: StateHandle[S], name: String = ""): Contact[S] =
      zipWithState(stateHolder) map(_._1, nextLabel(name, "_._1"))

    /** Converts data to pair with current state value. */
    def zipWithState[S](stateHolder: StateHandle[S], name: String = ""): Contact[(S, T)] =
      addLink(c, auxContact[(S, T)], StateZipLink[S, T, T](stateHolder, nextLabel(name, "(" + stateHolder + ", _)")))

    // Old way of zipWithState:
    //			(new LinkWithState(c, auxContact, stateHolder)).stateMap((s, t) ⇒ (s, (s, t)), nextLabel(name, "zip("+stateHolder+")"))
    def from[S](stateHolder: StateHandle[S], name: String = ""): Contact[(S, T)] =
      zipWithState(stateHolder, nextLabel(name, "from " + stateHolder))

    def directly[T2 >: T](c2: Contact[T2]) =
      addLink(c, c2, new NopLink[T, T2](nextLabel("", ">>")))

    def >>[T2 >: T](c2: Contact[T2], name: String = "") =
      addLink(c, c2, new NopLink[T, T2](nextLabel(name, ">>")))

    def delayOne: Contact[T] =
      new ImplDirectLinkBuilder[T,T](c, auxContact).directly(nextLabel("", "Δt"))

    /**
     * Create delay line that delays propagation of the signal by the given number of ticks.
     * For big counts there should be another implementation based on creating single special contact
     * and sending pair - (count, data) circulating until count == 0.
     */
    def delay(count: Int): Contact[T] = {
      def delay0(c: Contact[T], count: Int): Contact[T] = {
        if (count == 0)
          c
        else
          delay0(new ImplRichContact(c).delayOne, count - 1)
      }
      if (count < 0)
        throw new IllegalArgumentException("Cannot delay signal by negative number of ticks.")
      else
        delay0(c, count)
    }

    def delayCorrelated(c2: Contact[_]) = {
      val distance = minDistance(c, c2)
      if (distance == -1)
        throw new IllegalArgumentException(s"Contacts $c and $c2 are uncorrelated.")
      else
        delay(distance)
    }

    /** fires fast execution until the given finishContacts. Be careful. */
    def fireUntilSet[T2 <: T](start: Contact[T2], finishContacts: Set[Contact[_]], name: String = "") {
      addLink[T, T2](c, start, RedMapLink[T, T2](finishContacts + start, nextLabel(name, "fire")))
    }

    /** fires fast execution until the given finishContacts. Be careful. */
    def fire[T2 <: T](start: Contact[T2], finishContacts: Contact[_]*) {
      addLink[T, T2](c, start, RedMapLink[T, T2](finishContacts.toSet + start, nextLabel("", "fire")))
    }

    def ifConst(const: T, name: String = "") =
      c.filter(_ == const, nextLabel(name, "_ == " + const + "?"))

    def switcher(name: String = "") = new SwitcherBuilder(name)

    class SwitcherBuilder(name: String = "") {
      val defaultId = name + "Else"
      val selectorName = nextLabel(name, "selector")

      case class Condition(id: String, condition: T => Boolean)

      var completed = false
      val conditions = mutable.ListBuffer[Condition]()
      val endPoints = mutable.ListBuffer[Contact[_]]()
      val selector = auxContact[(String, T)]

      def If(condition: T => Boolean, name: String = "") = {
        require(conditions.size == 0)
        ElseIf(condition, name)
      }

      private def sCase(id: String) = {
        val res = selector.Case(id)
        endPoints += res
        res
      }

      def ElseIf(condition: T => Boolean, name: String = "") = {
        require(!completed, "the switcher " + name + " is completed.")
        val id = nextLabel(name, "" + conditions.size)
        conditions += Condition(id, condition)
        sCase(id)
      }

      def Else(name: String = "") = {
        require(!completed, "the switcher " + name + " is completed.")
        completed = true
        compileSelector()
        sCase(defaultId)
      }

      private def compileSelector() {
        completed = true
        val conditionsList = conditions.toList
        val preSelector = auxContact[(String, T)]
        (c -> preSelector).map(value => {
          val id = conditionsList.find(_.condition(value)).map(_.id).getOrElse(defaultId)
          (id, value)
        }, selectorName)
        preSelector.fireUntilSet(selector, endPoints.toSet)
      }
    }
  }
  implicit class ZippingLink[S, T](c:(Contact[T], Contact[(S,T)])){
    def zipWithState(stateHolder : StateHandle[S], name:String="") : Contact[(S, T)] =
      addLink(c._1, c._2, StateZipLink[S, T, T](stateHolder, nextLabel(name, "("+stateHolder+", _)")))
  }

  implicit class RichState[S](s:StateHandle[S]){
    def >>:(c: Contact[S]) = {
      c.saveTo(s)
      c
    }
  }
}

class SystemBuilderC(name : String) extends  SystemBuilder {
  this.setSystemName(name)
}
