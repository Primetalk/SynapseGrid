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
package ru.primetalk.synapse
package core

import scala.reflect.ClassTag
import scala.collection.{GenTraversableOnce, mutable}
import scala.language.implicitConversions
  /**
   * Doesn't work because T2 is unknown when it is called implicitly.
   * <pre>
   * implicit def contactToLink[T1, T2](c1:Contact[T1]) = {
   * val c2 = addContact(new Contact[T2](nextContactName, AuxiliaryContact))
   * new ImplLinkBuilder(c1, c2)
   * }
   * </pre>
   */
//  class ImplDirectLinkBuilder[T1, T2 >: T1](p: (Contact[T1], Contact[T2]))(implicit sb:BasicSystemBuilder) {
//    def directly(name: String = "Δt") =
//      sb.addLink(p._1, p._2, new NopLink[T1, T2](name))
//
//    def filter(predicate: T1 ⇒ Boolean, name: String = "") = //: FlatMapLink[T1, T2, Seq[T2]] =
//      sb.addLink(p._1, p._2,
//        new FlatMapLink[T1, T2]({
//          x ⇒
//            if (predicate(x))
//              Seq(x: T2)
//            else
//              Seq[T2]()
//        }, sb.nextLabel(name, if (name.endsWith("?")) name else name + "?"))) //.asInstanceOf[FlatMapLink[T1, T2, Seq[T2]]] //[T1, T2, MapLink[T1,T2]]
//  }
//
class AuxContactNumberingExt(val sb:BasicSystemBuilder) extends SystemBuilderExtension {
  private var auxContactNumber = 0
  def nextContactName = {
    sb.assertWritable()
    auxContactNumber += 1
    "c" + (auxContactNumber - 1)
  }
  def auxContact[T] =
    new Contact[T](nextContactName, AuxiliaryContact)
	
}
class LabellingExt(val sb:BasicSystemBuilder) extends SystemBuilderExtension {
	private[synapse] var proposedLabels = List[String]()
	 def labels(labels: String*) {
	    sb.assertWritable()	    
	    proposedLabels = labels.toList ::: proposedLabels	    
	  }

	def nextLabel(userProvidedLabel: String, defaultLabel: => String): String = {
		
		(userProvidedLabel, proposedLabels) match {
			case ("", List()) ⇒ defaultLabel
			case ("", head :: tail) ⇒
				sb.assertWritable()
				proposedLabels = tail
				head
			case (label, _) => label
		}
	}
}

class LinkBuilderOps[T1, T2](c: (Contact[T1], Contact[T2]))(sb: BasicSystemBuilder) {
	import core._
	def labelNext(label: String*) = {
		sb.labels(label: _*)
		this
	}

	def map(f: T1 ⇒ T2, name: String = ""): Contact[T2] =
		sb.addLink(c._1, c._2, new FlatMapLink[T1, T2](x => Seq(f(x)), sb.nextLabel(name, "" + f)))

	def const(value: T2, name: String = ""): Contact[T2] =
		sb.addLink(c._1, c._2, new FlatMapLink[T1, T2]((t: T1) => Seq(value), sb.nextLabel(name, "⇒" + value)))

	def flatMap[TSeq](f: T1 ⇒ GenTraversableOnce[T2], name: String = "") =
		sb.addLink(c._1, c._2, new FlatMapLink[T1, T2](f, sb.nextLabel(name, "" + f)))

	def splitToElements(name: String = "")(implicit ev: T1 <:< TraversableOnce[T2]): Contact[T2] =
		flatMap(t => ev(t), sb.nextLabel(name, "split"))

	def optionalMap(f: T1 ⇒ Option[T2], name: String = "") = //: FlatMapLink[T1, T2, Seq[T2]] =
		sb.addLink(c._1, c._2, new FlatMapLink[T1, T2](f(_).toSeq, sb.nextLabel(name, "" + f))) //.asInstanceOf[FlatMapLink[T1, T2, TSeq]] //[T1, T2, MapLink[T1,T2]]
	/** Cast data to the given class if possible */
	def castFilter[T3 <: T2](t2Class: Class[T3], name: String = "") = {
		sb.addLink(c._1, c._2,
			new FlatMapLink[T1, T2](
				d ⇒ if (t2Class.isInstance(d))
					Seq(d.asInstanceOf[T2])
				else
					Seq(),
				sb.nextLabel(name, "cast(" + t2Class.getSimpleName + ")")))
	}

	def castFilter2[T3 <: T2](implicit t3Class: ClassTag[T3]) = {
		sb.addLink(c._1, c._2,
			new FlatMapLink[T1, T2]({
				case t3Class(d) => Seq(d.asInstanceOf[T2])
				case _ => Seq()
			},
				sb.nextLabel("", "cast2(" + t3Class.runtimeClass.getSimpleName + ")")))
	}

	def collect(f: PartialFunction[T1, T2], name: String = "") =
		flatMap(t => {
			if (f.isDefinedAt(t)) Seq(f(t)) else Seq()
		}, name)

	def stateMap[S](stateHandle: StateHandle[S], name: String = "")(f: (S, T1) ⇒ (S, T2)) =
		sb.addLink(c._1, c._2,
			new StatefulFlatMapLink[S, T1, T2](
				(s, t) => { val r = f(s, t); (r._1, Seq(r._2)) }, stateHandle,
				sb.nextLabel(name, "sm")))
	def stateFlatMap[S](stateHandle: StateHandle[S], name: String = "")(f: (S, T1) ⇒ (S, GenTraversableOnce[T2])) =
		sb.addLink(c._1, c._2, new StatefulFlatMapLink[S, T1, T2](f, stateHandle, sb.nextLabel(name, "sfm")))

}
class StateLinkBuilder2Ops[T1, T2, S](p:(core.ContactWithState[T1, S], Contact[T2]))(sb:BasicSystemBuilder) {
    def stateMap(f: (S, T1) ⇒ (S, T2), name:String ="") =
      sb.addLink(p._1.c1, p._2,
        new StatefulFlatMapLink[S, T1, T2](
          (s, t) => {val r = f(s,t);(r._1, Seq(r._2))}, p._1.stateHandle,
        sb.nextLabel(name, "sm")))
    def stateFlatMap(f: (S, T1) ⇒ (S, GenTraversableOnce[T2]), name:String ="") =
      sb.addLink(p._1.c1, p._2, new StatefulFlatMapLink[S,T1, T2](f, p._1.stateHandle,
        sb.nextLabel(name, "sfm")))
}

class DirectLinkBuilderOps[T1, T2 >: T1](p: (Contact[T1], Contact[T2]))(sb: BasicSystemBuilder) {
	def directly(name: String = "Δt") =
		sb.addLink(p._1, p._2, new NopLink[T1, T2](name))

	def filter(predicate: T1 ⇒ Boolean, name: String = "") = //: FlatMapLink[T1, T2, Seq[T2]] =
		sb.addLink(p._1, p._2,
			new FlatMapLink[T1, T2]({
				x ⇒
					if (predicate(x))
						Seq(x: T2)
					else
						Seq[T2]()
			}, sb.nextLabel(name, if (name.endsWith("?")) name else name + "?"))) //.asInstanceOf[FlatMapLink[T1, T2, Seq[T2]]] //[T1, T2, MapLink[T1,T2]]
}
class ContactWithState[T1, S](val c1: Contact[T1], val stateHandle: StateHandle[S])(sb: BasicSystemBuilder) {
	def stateMap[T2](f: (S, T1) ⇒ (S, T2), name: String = "") =
		sb.addLink(c1, sb.auxContact[T2],
			new StatefulFlatMapLink[S, T1, T2](
				(s, t) => { val r = f(s, t); (r._1, Seq(r._2)) }, stateHandle,
				sb.nextLabel(name, "sm")))
	def stateFlatMap[T2](f: (S, T1) ⇒ (S, GenTraversableOnce[T2]), name: String = "") =
		sb.addLink(c1, sb.auxContact[T2], new StatefulFlatMapLink[S, T1, T2](f, stateHandle,
			sb.nextLabel(name, "sfm")))

	def updateState(name: String = "")(fun: (S, T1) ⇒ S) {
		sb.addComponent(new StateUpdate[S, T1](c1, stateHandle, sb.nextLabel(name, "update(" + fun + "," + stateHandle + ")"), fun))
	}

}
class ContactPairOps[S, T](c : Contact[(S, T)])(sb:BasicSystemBuilder) {
    require(c!= null, "Contact is null")

	implicit def implDirectLinkBuilder[T1, T2 >: T1](p: (Contact[T1], Contact[T2])) = new core.DirectLinkBuilderOps(p)(sb)
	implicit def implLinkBuilder[T1, T2](c: (Contact[T1], Contact[T2])) = new core.LinkBuilderOps(c)(sb)
    /** Converts data to pair with current state value. */
    def unzipWithState(stateHandle : StateHandle[S], name:String = "") : Contact[T] = {
      (c -> sb.auxContact[T]) .stateMap(stateHandle, sb.nextLabel(name, "unzip to "+stateHandle)) ((s, p:(S, T)) ⇒ (p._1, p._2))
    }
    /** Switches based on the first element of the pair.*/
    def Case(CaseValue:S):Contact[T] = {
      c -> sb.auxContact[T] collect ({ case (CaseValue, value) => value },s"Case($CaseValue)")
    }
}

class ZippingLinkOps[S, T](c:(Contact[T], Contact[(S,T)]))(sb:BasicSystemBuilder) {
    def zipWithState(stateHolder : StateHandle[S], name:String="") : Contact[(S, T)] =
      sb.addLink(c._1, c._2, StateZipLink[S, T, T](stateHolder, sb.nextLabel(name, "("+stateHolder+", _)")))
}

class ContactOps[T](val c: Contact[T])(sb:BasicSystemBuilder) {
    require(c != null, "Contact is null")
    
    def labelNext(label: String*) = {
      sb.labels(label: _*)
      c
    }


    def output() {
      sb.outputs(c)
    }

    def input: Contact[T] = {
      sb.inputs(c)
      c
    }
	implicit def implDirectLinkBuilder[T1, T2 >: T1](p: (Contact[T1], Contact[T2])) = new core.DirectLinkBuilderOps(p)(sb)
	implicit def implLinkBuilder[T1, T2](c: (Contact[T1], Contact[T2])) = new core.LinkBuilderOps(c)(sb)
    /** Declares the first contact as input and creates link to the second*/
    def inputMappedTo[T2 >: T](c2:Contact[T2]) = {
      sb.inputs(c)
      >>(c2)
      c2
    }
    /** Declares the second contact as output and creates link from the first*/
    def mapToOutput[T2 >: T](c2:Contact[T2]){
      sb.outputs(c2)
      >> (c2)
    } 

    def >>[T2 >: T](c2: Contact[T2], name: String = "") =
      (c, c2).directly(sb.nextLabel(name, ">>"))

	def stock(f: T ⇒ Any, name: String = "") = {
		(c -> core.devNull).flatMap(x => Seq(f(x)), sb.nextLabel(name, ">>null"))
		c
	}

    def foreach(body: T ⇒ Any, name: String = "") = {
      sb.addLink(c, devNull, new FlatMapLink[T, Any](x=>{body(x);Seq()}, sb.nextLabel(name, "foreach")))
      c
    }

    def exec(body: ⇒ Any, name: String = "") = {
      sb.addLink(c, sb.auxContact[T], new FlatMapLink[T, T]((t: T) => {
        body; Seq(t)
      }, sb.nextLabel(name, "exec")))
    }

    
    /**
     * Filters the data from this contact. Returns another contact that will get filtered data
     */
    def filter(predicate: T ⇒ Boolean, name: String = ""): Contact[T] =
    	(c -> sb.auxContact[T]).filter(predicate, sb.nextLabel(name, "" + predicate + "?"))

    /**
     * Filters the data from this contact. Returns another contact that will get filtered data
     */
    def withFilter(predicate: T ⇒ Boolean): Contact[T] =
      filter(predicate)



    def mapTo[T2](f: T ⇒ T2, auxContact1: Contact[T2] = sb.auxContact[T2]): Contact[T2] =
      (c, auxContact1).map(f, sb.nextLabel("", "mapTo(" + f + ")"))

    def map[T2](f: T ⇒ T2, name: String = ""): Contact[T2] =
      (c, sb.auxContact[T2]).map(f, sb.nextLabel(name, "map(" + f + ")"))

    def const[T2](value: T2, name: String = ""): Contact[T2] =
      (c, sb.auxContact[T2]).map(t => value, sb.nextLabel(name, "⇒" + value))

    def castFilter2[T3<: T](implicit t3Class: ClassTag[T3]) =
      (c, sb.auxContact[T3]).castFilter(t3Class.runtimeClass.asInstanceOf[Class[T3]])
    
    def castFilter[T3](t3Class: Class[T3], name: String = "") =
      (c, sb.auxContact[T3]).castFilter(t3Class)

    def collect[T2](f: PartialFunction[T, T2], name: String = "") =
      (c, sb.auxContact[T2]).collect(f, name)

    def flatMap[T2](f: T ⇒ TraversableOnce[T2], name: String = ""): Contact[T2] =
      (c, sb.auxContact[T2]).flatMap(f, sb.nextLabel(name, "fM(" + f + ")"))

    def splitToElements[T2](name: String = "")(implicit ev: T <:< TraversableOnce[T2]): Contact[T2] =
      (c, sb.auxContact[T2]).splitToElements(name)//flatMap(t => ev(t), nextLabel(name, "split"))

    /** Converts data to pair with current state value. */
    def zipWithConst[T2](value: T2, name: String = ""): Contact[(T2, T)] =
      map((value, _), sb.nextLabel(name, s"($value, _)"))

      
      
      
      
    /** Update state in state handle. */

    def updateState[S](stateHandle: StateHandle[S], name: String = "")(fun: (S, T) ⇒ S) {
      sb.addComponent(new StateUpdate[S, T](c, stateHandle, sb.nextLabel(name, "update(" + fun + "," + stateHandle + ")"), fun))
    }

    def inc[S:Numeric](stateHandle: StateHandle[S], name: String = "") {//(implicit ev: S <:< Numeric[S], n : Numeric[S])
      val n = implicitly[Numeric[S]]
      sb.addComponent(new StateUpdate[S, T](c, stateHandle,
        sb.nextLabel(name, "inc(" + stateHandle + ")"), (s, _) => n.plus(s , n.one)))
    }

    def dec[S:Numeric](stateHandle: StateHandle[S], name: String = "") {
      val n = implicitly[Numeric[S]]
      sb.addComponent(new StateUpdate[S, T](c, stateHandle,
        sb.nextLabel(name, "dec(" + stateHandle + ")"), (s, _) => n.minus(s , n.one)))
    }

    def addTo[S](stateHandle: StateHandle[S], name: String = "")(implicit n:Numeric[S], ev:T <:< S) {
      sb.addComponent(new StateUpdate[S, T](c, stateHandle,
        sb.nextLabel(name, "addTo(" + stateHandle + ")"),
          (s, a) =>n.plus(s , a)
          ))
    }
    def withState[S](stateHandle: StateHandle[S]) = new core.ContactWithState[T, S](c, stateHandle)(sb)

    def saveTo[S >: T](stateHolder: StateHandle[S], name: String = "") {
      sb.addComponent(new StateUpdate[S, T](c, stateHolder, sb.nextLabel(name, "saveTo(" + stateHolder + ")"), (s, t) ⇒ t))
    }

    def prependList[S >: T](stateHolder: StateHandle[List[S]], name: String = "") {
      updateState(stateHolder, sb.nextLabel(name, "addToList(" + stateHolder + ")"))((list, item) ⇒ item :: list)
    }

    def clearList[S](stateHolder: StateHandle[List[S]], name: String = "") {
      updateState(stateHolder, sb.nextLabel(name, "clearList(" + stateHolder + ")"))((list, item) ⇒ Nil)
    }

    def setState[S](stateHandle: StateHandle[S], name: String = "")(fun: T ⇒ S) = {
      new core.LinkBuilderOps(c, devNull)(sb).stateFlatMap(
        stateHandle, sb.nextLabel(name, "" + stateHandle + " := setState(" + fun + ")"))
      {(s: S, t: T) ⇒ (fun(t), Seq())}
    }

    def resetState[S](stateHandle: StateHandle[S], name: String = "") = 
     setState(stateHandle, sb.nextLabel(name, "" + stateHandle + " := s0"))(_ => stateHandle.s0)      
      
     
     
     
    def passByStateCondition[S](stateHandle : StateHandle[S], name: String = "")(condition:S=>Boolean):Contact[T] = {
      val res = sb.auxContact[T]
      (c -> res).stateFlatMap(stateHandle, sb.nextLabel(name, "pass if condition on "+stateHandle.name))( (s,t) => if(condition(s))(s,Seq(t))else(s,Seq()) )
      res
    }
    def passByStateConditionAndUpdateState[S](stateHandle : StateHandle[S], name: String = "")(condition:(S, T)=>Option[S]):Contact[T] = {
      val res = sb.auxContact[T]
      (c -> res).stateFlatMap(stateHandle, sb.nextLabel(name, "pass if condition on "+stateHandle.name)){ (s,t) => val v = condition(s, t); if(v.isDefined)(v.get,Seq(t))else(s,Seq()) }
      res
    }
    def passIfEnabled(stateHandle : StateHandle[Boolean], name: String = "") =
      passByStateCondition(stateHandle, sb.nextLabel(name, "pass if "+stateHandle.name+"?")) (identity)

    def activate(stateHolder: StateHandle[Boolean], isActive: Boolean = true) = {
      labelNext("⇒" + isActive)
      val c2 = (c, sb.auxContact[Boolean]).map(_ ⇒ isActive)
      new core.ContactOps(c2)(sb).saveTo(stateHolder)
      c
    }

    def deactivate(stateHolder: StateHandle[Boolean]) = {
      activate(stateHolder, isActive = false)
      c
    }

    /** Extracts current state value. */
    def getState[S](stateHolder: StateHandle[S], name: String = ""): Contact[S] =
      (zipWithState(stateHolder) -> sb.auxContact[S]). map(_._1, sb.nextLabel(name, "_._1"))

    implicit def zippingLink[S](c:(Contact[T], Contact[(S,T)]))(sb:BasicSystemBuilder) = new ZippingLinkOps[S, T](c:(Contact[T], Contact[(S,T)]))(sb)
    /** Converts data to pair with current state value. */
    def zipWithState[S](stateHolder: StateHandle[S], name: String = ""): Contact[(S, T)] =
    	new ZippingLinkOps(c -> sb.auxContact[(S,T)])(sb).zipWithState(stateHolder, name)

    def from[S](stateHolder: StateHandle[S], name: String = ""): Contact[(S, T)] =
      zipWithState(stateHolder, sb.nextLabel(name, "from " + stateHolder))

      
      
      
    /**
     * Latch is a state of type Option[S]. It can be cleared to None by one signal,
     * and set to Some() by another signal. After setting it doesn't change until cleared.
     */
    def clearLatch[S](stateHolder: StateHandle[Option[S]]) {
      val c2 = sb.auxContact[Option[S]]
      (new ContactOps(c)(sb).labelNext("⇒None"), c2).map(any ⇒ None)
      new ContactOps(c2)(sb).saveTo(stateHolder)
    }

    /** Sets latch value it it was not set yet */
    def latchValue[S >: T](stateHolder: StateHandle[Option[S]], f: T ⇒ S = identity[T](_)) = {
      new core.LinkBuilderOps(c, devNull)(sb).stateFlatMap(stateHolder, sb.nextLabel("", "" + stateHolder + "<?=Some"))
      {(s: Option[S], t: T) ⇒ (if (s.isEmpty) Some(f(t)) else s, Seq())}

    }

    
    
    
    
    /** fires fast execution until the given finishContacts. Be careful. */
    def fireUntilSet[T2 <: T](start: Contact[T2], finishContacts: Set[Contact[_]], name: String = "") {
      sb.addLink[T, T2](c, start, RedMapLink[T, T2](finishContacts + start, sb.nextLabel(name, "fire")))
    }

    /** fires fast execution until the given finishContacts. Be careful. */
    def fire[T2 <: T](start: Contact[T2], finishContacts: Contact[_]*) {
      sb.addLink[T, T2](c, start, RedMapLink[T, T2](finishContacts.toSet + start, sb.nextLabel("", "fire")))
    }

    
    
    
    
    def delayOne: Contact[T] =
      (c, sb.auxContact[T]).directly(sb.nextLabel("", "Δt"))
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
          delay0(new ContactOps(c)(sb).delayOne, count - 1)
      }
      if (count < 0)
        throw new IllegalArgumentException("Cannot delay signal by negative number of ticks.")
      else
        delay0(c, count)
    }

    def delayCorrelated(c2: Contact[_]) = {
      val distance = sb.minDistance(c, c2)
      if (distance == -1)
        throw new IllegalArgumentException(s"Contacts $c and $c2 are uncorrelated.")
      else
        delay(distance)
    }


    
    
    def directly[T2 >: T](c2: Contact[T2]) =
      (c, c2).directly(sb.nextLabel("", ">>"))



    def ifConst(const: T, name: String = "") =
      filter(_ == const, sb.nextLabel(name, "_ == " + const + "?"))

    def switcher(name: String = "") = 
    	new core.SwitcherBuilder[T](c, name)(sb)
    
}
class StateOps[S](s:StateHandle[S])(sb:BasicSystemBuilder){
    def >>:(c: Contact[S]) = {
      new ContactOps(c)(sb).saveTo(s)
      c
    }
}
trait SystemBuilderAdv {
	def sb:BasicSystemBuilder
  // TODO: macros like: `state counterS:Int = 0` and `contact myContact:String`

//	import sb._
	import core._
	def nextContactName =
		sb.extend(AuxContactNumberingExtId).nextContactName

	/**
	 * Defines the sequence of labels to be used for superscription of links.
	 */
	def labels(labels: String*) = {
		sb.extend(LabellingExtId).labels(labels: _*)
		this
	}

	private[synapse] def nextLabel(userProvidedLabel: String, defaultLabel: => String): String = {
		val lsb = sb.extend(LabellingExtId)
		(userProvidedLabel, lsb.proposedLabels) match {
			case ("", List()) ⇒ defaultLabel
			case ("", head :: tail) ⇒
				sb.assertWritable()
				lsb.proposedLabels = tail
				head
			case (label, _) => label
		}
	}
	/**
	 * Create contact and add it to the builder
	 */
	def contact[T](name: String) =
		core.contact[T](name)

	def auxContact[T] = sb.auxContact[T]
  /**
   * Special contact for consuming unnecessary data values.
   */
  def devNull = core.devNull
  /**
   * Create contact and add it to the builder
   */
  def input[T](name: String) = {
    val c = contact[T](name)
    sb.inputs(c)
    c
  }
  /**
   * Create contact and add it to the builder
   */
  def output[T](name: String) = {
    val c = contact[T](name)
    sb.outputs(c)
    c
  }


  def connect[T1, T2 >:T1](c1:Contact[T1], c2:Contact[T2]) {
    c1 >> c2
  }


  /** Declares the first contact as input and creates link to the second*/
  def mappedInput[T, T2 >: T](c1:Contact[T], c2:Contact[T2]) = {
    sb.inputs(c1)
    c1 >> c2
    c2
  }
  /** Declares the second contact as output and creates link from the first*/
  def mappedOutput[T, T2 >: T](c1:Contact[T], c2:Contact[T2]){
    sb.outputs(c2)
    c1 >> c2
  }
	/*
   * Doesn't work because T2 is unknown when it is called implicitly.
   * <pre>
   * implicit def contactToLink[T1, T2](c1:Contact[T1]) = {
   * val c2 = addContact(new Contact[T2](nextContactName, AuxiliaryContact))
   * new ImplLinkBuilder(c1, c2)
   * }
   * </pre>
   */
	implicit def implDirectLinkBuilder[T1, T2 >: T1](p: (Contact[T1], Contact[T2])) = new core.DirectLinkBuilderOps(p)(sb)
	implicit def implLinkBuilder[T1, T2](c: (Contact[T1], Contact[T2])) = new core.LinkBuilderOps(c)(sb)

	implicit def implRichContactPair[S, T](c: Contact[(S, T)]) = new core.ContactPairOps(c)(sb)
	implicit def zippingLink[S, T](c: (Contact[T], Contact[(S, T)])) = new core.ZippingLinkOps[S, T](c: (Contact[T], Contact[(S, T)]))(sb)

	implicit def stateLinkBuilder2Ops[T1, T2, S](p: (core.ContactWithState[T1, S], Contact[T2])) = new core.StateLinkBuilder2Ops(p)(sb)

	implicit def richState[S](s: StateHandle[S]) = new core.StateOps(s)(sb)
	implicit def contactOps[T](c: core.Contact[T]): core.ContactOps[T] = new core.ContactOps(c)(sb)
}

class SwitcherBuilder[T](c: Contact[T], name: String = "")(sb: BasicSystemBuilder) {
	val defaultId = name + "Else"
	val selectorName = sb.nextLabel(name, "selector")

	case class Condition(id: String, condition: T => Boolean)

	var completed = false
	val conditions = mutable.ListBuffer[Condition]()
	val endPoints = mutable.ListBuffer[Contact[_]]()
	val selector = sb.auxContact[(String, T)]

	def If(condition: T => Boolean, name: String = "") = {
		require(conditions.size == 0)
		ElseIf(condition, name)
	}

	private def sCase(id: String) = {
		val res = new ContactPairOps(selector)(sb).Case(id)
		endPoints += res
		res
	}

	def ElseIf(condition: T => Boolean, name: String = "") = {
		require(!completed, "the switcher " + name + " is completed.")
		val id = sb.nextLabel(name, "" + conditions.size)
		conditions += Condition(id, condition)
		sCase(id)
	}

	def Else(name: String = "") = {
		require(!completed, "the switcher " + name + " is completed.")
		completed = true
		compileSelector()
		sCase(defaultId)
	}

	implicit def implLinkBuilder[T1, T2](c: (Contact[T1], Contact[T2])) = new core.LinkBuilderOps(c)(sb)
	private def compileSelector() {
		completed = true
		val conditionsList = conditions.toList
		val preSelector = sb.auxContact[(String, T)]
		(c -> preSelector).map(value => {
			val id = conditionsList.find(_.condition(value)).map(_.id).getOrElse(defaultId)
			(id, value)
		}, selectorName)
		new ContactOps(preSelector)(sb).fireUntilSet(selector, endPoints.toSet)
	}
}
/** DSL for constructing systems */
trait SystemBuilder extends BasicSystemBuilder with SystemBuilderAdv {
	override
	def sb:BasicSystemBuilder = this
}
class SystemBuilderC(name : String) extends  SystemBuilder {
  this.setSystemName(name)
}

class SystemBuilderAdvC(val sb:BasicSystemBuilder) extends SystemBuilderAdv 
