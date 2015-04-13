package ru.primetalk.synapse.core.dsl

import ru.primetalk.synapse.core.components.StateUpdate

import scala.collection.GenTraversableOnce
import scala.reflect.ClassTag

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
//TODO:[ ] поддержка timeout exception - и особая обработка. Возможность создания future и ожидания результата с таймаутом. mapFuture
//TODO:[ ] создание вокруг подсистемы механизма Try и обработка исключений на уровне родительской системы

trait SystemBuilderDslApi extends SystemBuilderApi with NextLabelExt with AuxNumberingExt with DevNullExt {

  /**
   * DSL methods for creating links between the two given contacts.*/
  implicit class LinkBuilderOps[T1, T2](pair: (Contact[T1], Contact[T2]))(implicit sb: SystemBuilder) {

    def labelNext(label: String*) = {
      sb.labels(label: _*)
      this
    }

    def map(f: T1 ⇒ T2, name: String = ""): Contact[T2] =
      sb.addLink(pair._1, pair._2, sb.nextLabel(name, "" + f),
        new FlatMapLink[T1, T2](x => Seq(f(x))))

    def const(value: T2, name: String = ""): Contact[T2] =
      sb.addLink(pair._1, pair._2, sb.nextLabel(name, "⇒" + value),
        new FlatMapLink[T1, T2]((t: T1) => Seq(value)))

    def flatMap[TSeq](f: T1 ⇒ GenTraversableOnce[T2], name: String = ""): Contact[T2] =
      sb.addLink(pair._1, pair._2, sb.nextLabel(name, "" + f),
        new FlatMapLink[T1, T2](f))

    def flatMapOpt(f: T1 ⇒ Option[T2], name: String = ""): Contact[T2] = //: FlatMapLink[T1, T2, Seq[T2]] =
      flatMap(f(_).toSeq, name)

    @deprecated("use #flatMapOpt", "13.04.2015")
    def optionalMap(f: T1 ⇒ Option[T2], name: String = ""): Contact[T2] = //: FlatMapLink[T1, T2, Seq[T2]] =
      flatMapOpt(f, name)

    //      sb.addLink(c._1, c._2, sb.nextLabel(name, "" + f),
    //        new FlatMapLink[T1, T2](f(_).toSeq)) //.asInstanceOf[FlatMapLink[T1, T2, TSeq]] //[T1, T2, MapLink[T1,T2]]
    //  // this variant of flatMap is conflicting with flatMap-GenTraversableOnce.
    //  // It seems converting Option to Traversable is better.
    //  def flatMap(f: T1 ⇒ Option[T2], name: String = "") = //: FlatMapLink[T1, T2, Seq[T2]] =
    //    sb.addLink(c._1, c._2, sb.nextLabel(name, "" + f),
    //      new FlatMapLink[T1, T2](f(_).toSeq)) //.asInstanceOf[FlatMapLink[T1, T2, TSeq]] //[T1, T2, MapLink[T1,T2]]

    @deprecated("use #flatten", "13.04.2015")
    def splitToElements(name: String = "")(implicit ev: T1 <:< GenTraversableOnce[T2]): Contact[T2] =
      flatten(name)


    def flatten(name: String = "")(implicit ev: T1 <:< GenTraversableOnce[T2]): Contact[T2] =
      flatMap((t: T1) => ev(t), sb.nextLabel(name, "flatten"))


    /** Cast data to the given class if possible. If the data cannot be cast, then it is filtered out.
      * Prefer to use castFilter2
      */
    def castFilter[T3 <: T2](t2Class: Class[T3], name: String = "") = {
      sb.addLink(pair._1, pair._2,
        sb.nextLabel(name, "cast(" + t2Class.getSimpleName + ")"),
        new FlatMapLink[T1, T2](
          d ⇒ if (t2Class.isInstance(d))
            Seq(d.asInstanceOf[T2])
          else
            Seq()))
    }

    /** Cast data to the given class if possible. If the data cannot be cast, then it is filtered out.
      * This method is preferred because it is less boilerplate. It is often enough to simply mention
      * the type parameter.
      */
    def castFilter2[T3 <: T2](implicit t3Class: ClassTag[T3]) = {
      sb.addLink(pair._1, pair._2,
        sb.nextLabel("", "cast2(" + t3Class.runtimeClass.getSimpleName + ")"),
        new FlatMapLink[T1, T2]({
          case t3Class(d) => Seq(d)
          case _ => Seq()
        }))
    }

    def collect(f: PartialFunction[T1, T2], name: String = "") =
      flatMap((t: T1) => {
        if (f.isDefinedAt(t)) Seq(f(t)) else Seq()
      }, name)

    def stateMap[S](stateHandle: StateHandle[S], name: String = "")(f: (S, T1) ⇒ (S, T2)) =
      sb.addLink(pair._1, pair._2,
        sb.nextLabel(name, "sm"),
        new StatefulFlatMapLink[S, T1, T2](
          (s, t) => {
            val r = f(s, t)
            (r._1, Seq(r._2))
          }, stateHandle))

    def stateFlatMap[S](stateHandle: StateHandle[S], name: String = "")(f: (S, T1) ⇒ (S, GenTraversableOnce[T2])) =
      sb.addLink(pair._1, pair._2, sb.nextLabel(name, "sfm"),
        new StatefulFlatMapLink[S, T1, T2](f, stateHandle))

  }


  implicit class DirectLinkBuilderOps[T1, T2 >: T1](p: (Contact[T1], Contact[T2]))(implicit sb: SystemBuilder) {
    def directly(name: String = "Δt") =
      sb.addLink(p._1, p._2, name, new NopLink[T1, T2]())

    def filter(predicate: T1 ⇒ Boolean, name: String = "") = //: FlatMapLink[T1, T2, Seq[T2]] =
      sb.addLink(p._1, p._2,
        sb.nextLabel(name, if (name endsWith "?") name else name + "?"),
        new FlatMapLink[T1, T2]({
          x ⇒
            if (predicate(x))
              Seq(x: T2)
            else
              Seq[T2]()
        }))

    //.asInstanceOf[FlatMapLink[T1, T2, Seq[T2]]] //[T1, T2, MapLink[T1,T2]]
    def filterNot(predicateInv: T1 ⇒ Boolean, name: String = "") = //: FlatMapLink[T1, T2, Seq[T2]] =
      sb.addLink(p._1, p._2,
        sb.nextLabel(name, if (name endsWith "?") name else name + "?"),
        new FlatMapLink[T1, T2]({
          x ⇒
            if (!predicateInv(x))
              Seq(x: T2)
            else
              Seq[T2]()
        })) //.asInstanceOf[FlatMapLink[T1, T2, Seq[T2]]] //[T1, T2, MapLink[T1,T2]]
  }

  class ContactWithState[T1, S](val c1: Contact[T1], val stateHandle: StateHandle[S])(implicit sb: SystemBuilder) {

    def stateMap[T2](f: (S, T1) ⇒ (S, T2), name: String = "") =
      sb.addLink(c1, sb.auxContact[T2],
        sb.nextLabel(name, "sm"),
        new StatefulFlatMapLink[S, T1, T2](
          (s, t) => {
            val r = f(s, t)
            (r._1, Seq(r._2))
          }, stateHandle))

    def stateFlatMap[T2](f: (S, T1) ⇒ (S, GenTraversableOnce[T2]), name: String = "") =
      sb.addLink(c1, sb.auxContact[T2],
        sb.nextLabel(name, "sfm"),
        new StatefulFlatMapLink[S, T1, T2](f, stateHandle))

    def updateState(name: String = "")(fun: (S, T1) ⇒ S) {
      sb.addComponent(new StateUpdate[S, T1](c1, stateHandle, sb.nextLabel(name, "update(" + fun + "," + stateHandle + ")"), fun))
    }

  }

  /** Miscellaneous operations with prefixed data.
    */
  implicit class ContactPairOps[Key, T](c: Contact[(Key, T)])(implicit sb: SystemBuilder) {
    require(c != null, "Contact is null")

    //
    //    implicit def implDirectLinkBuilder[T1, T2 >: T1](p: (Contact[T1], Contact[T2])): DirectLinkBuilderOps[T1, T2] = new DirectLinkBuilderOps(p)(sb)
    //
    //    implicit def implLinkBuilder[T1, T2](c: (Contact[T1], Contact[T2])): LinkBuilderOps[T1, T2] = new LinkBuilderOps(c)(sb)
    //
    /** Converts data to pair with current state value. */
    def unzipWithState(stateHandle: StateHandle[Key], name: String = ""): Contact[T] = {
      (c -> sb.auxContact[T]).stateMap(stateHandle, sb.nextLabel(name, "unzip to " + stateHandle))((s, p: (Key, T)) ⇒ (p._1, p._2))
    }

    /** Switches based on the first element of the pair. */
    def Case(CaseValue: Key): Contact[T] = {
      c -> sb.auxContact[T] collect( {
        case (CaseValue, value) => value
      }, s"Case($CaseValue)")
    }

    /** Decrements the first argument by step = -1
      * until zero. At zero - stops decrementing.
      * This mechanism allows to create arbitrary delay lines. See delayN
      * */
    def countDown(step: Int = -1, name: String = "")(implicit num: Numeric[Key]): Contact[(Key, T)] = {
      (c -> c).flatMap({ case (s, t) =>

        if (num.zero == s)
          Seq()
        else
          Seq((num.plus(s, num.fromInt(step)), t))
      }, sb.nextLabel(name, "countDown"))
    }

    /** Checks if the key is 0.*/
    def isZeroCase(name: String = "")(implicit num: Numeric[Key]): Contact[T] = {
      c.flatMap({
        case (s, t) if num.zero == s => Seq(t)
        case _ => Seq()
      }, sb.nextLabel(name, "isZeroCase?"))
    }
  }

  implicit class ZippingLinkOps[S, T](c: (Contact[T], Contact[(S, T)]))(implicit sb: SystemBuilder) {

    def zipWithState(stateHolder: StateHandle[S], name: String = ""): Contact[(S, T)] =
      sb.addLink(c._1, c._2,
        sb.nextLabel(name, "(" + stateHolder + ", _)"),
        StateZipLink[S, T, T](stateHolder))
  }

  /** New methods available on contacts that construct links.
    */
  implicit class ContactOps[T](val c: Contact[T])(implicit sb: SystemBuilder) {
    require(c != null, "Contact is null. " +
      "This can usually happen when the contact is declared using val, " +
      "but it is placed further down the source code and thus has not been initialized yet.")

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

    //
    //    implicit def implDirectLinkBuilder[T1, T2 >: T1](p: (Contact[T1], Contact[T2])): DirectLinkBuilderOps[T1, T2] = new DirectLinkBuilderOps(p)(sb)
    //
    //    implicit def implLinkBuilder[T1, T2](c: (Contact[T1], Contact[T2])): LinkBuilderOps[T1, T2] = new LinkBuilderOps(c)(sb)
    //
    /** Declares the first contact as input and creates link to the second */
    def inputMappedTo[T2 >: T](c2: Contact[T2]) = {
      sb.inputs(c)
      >>(c2)
      c2
    }

    /** Declares the second contact as output and creates link from the first */
    def mapToOutput[T2 >: T](c2: Contact[T2]) {
      sb.outputs(c2)
      >>(c2)
    }

    def >>[T2 >: T](c2: Contact[T2], name: String = "") =
      (c, c2).directly(sb.nextLabel(name, ">>"))

    // stock, foreach and exec are similar in that these methods execute some arbitrary function.
    // The difference is only in the signature of the user function.

    def stock(f: T ⇒ Any, name: String = "") = {
      (c -> devNull).flatMap((x: T) => Seq(f(x)), sb.nextLabel(name, ">>null"))
      c
    }

    def foreach(body: T ⇒ Any, name: String = "") = {
      sb.addLink(c, devNull, sb.nextLabel(name, "foreach"),
        new FlatMapLink[T, Any](x => {
          body(x)
          Seq()
        }))
      c
    }

    /** Executes some code with side effects and passes the input data further. */
    def exec(body: ⇒ Any, name: String = "") = {
      sb.addLink(c, sb.auxContact[T], sb.nextLabel(name, "exec"),
        new FlatMapLink[T, T]((t: T) => {
          body
          Seq(t)
        }))
    }


    /**
     * Filters the data from this contact. Returns another contact that will contain filtered data
     */
    def filter(predicate: T ⇒ Boolean, name: String = ""): Contact[T] =
      (c -> sb.auxContact[T]).filter(predicate, sb.nextLabel(name, "" + predicate + "?"))

    def filterNot(predicateInv: T ⇒ Boolean, name: String = ""): Contact[T] =
      (c -> sb.auxContact[T]).filterNot(predicateInv, sb.nextLabel(name, "!" + predicateInv + "?"))

    /**
     * Filters the data from this contact. Returns another contact that will get filtered data.
     */
    def withFilter(predicate: T ⇒ Boolean): Contact[T] =
      filter(predicate)


    /** Creates another contact and links it to this one with transformation f. */
    def map[T2](f: T ⇒ T2, name: String = ""): Contact[T2] =
      (c, sb.auxContact[T2]).map(f, sb.nextLabel(name, "map(" + f + ")"))

    def mapTo[T2](f: T ⇒ T2, auxContact1: Contact[T2] = sb.auxContact[T2]): Contact[T2] =
      (c, auxContact1).map(f, sb.nextLabel("", "mapTo(" + f + ")"))

    /** Replaces every input item with the provided constant. */
    def const[T2](value: T2, name: String = ""): Contact[T2] =
      (c, sb.auxContact[T2]).map(t => value, sb.nextLabel(name, "⇒" + value))

    def castFilter2[T3 <: T](implicit t3Class: ClassTag[T3]) =
      (c, sb.auxContact[T3]).castFilter(t3Class.runtimeClass.asInstanceOf[Class[T3]])

    def castFilter[T3](t3Class: Class[T3], name: String = "") =
      (c, sb.auxContact[T3]).castFilter(t3Class)

    def collect[T2](f: PartialFunction[T, T2], name: String = "") =
      (c, sb.auxContact[T2]).collect(f, name)

    def flatMap[T2](f: T ⇒ TraversableOnce[T2], name: String = ""): Contact[T2] =
      (c, sb.auxContact[T2]).flatMap(f, sb.nextLabel(name, "fM(" + f + ")"))

    @deprecated("use #flatten", "13.04.2015")
    def splitToElements[T2](name: String = "")(implicit ev: T <:< TraversableOnce[T2]): Contact[T2] =
      (c, sb.auxContact[T2]).splitToElements(name) //flatMap(t => ev(t), nextLabel(name, "split"))

    def flatten[T2](name: String = "")(implicit ev: T <:< TraversableOnce[T2]): Contact[T2] =
      (c, sb.auxContact[T2]).flatten(name) //flatMap(t => ev(t), nextLabel(name, "split"))

    /** Converts data to pair with current state value. */
    def zipWithConst[T2](value: T2, name: String = ""): Contact[(T2, T)] =
      map((value, _), sb.nextLabel(name, s"($value, _)"))


    /** Update state in state handle. */

    def updateState[S](stateHandle: StateHandle[S], name: String = "")(fun: (S, T) ⇒ S) {
      sb.addComponent(new StateUpdate[S, T](c, stateHandle, sb.nextLabel(name, "update(" + fun + "," + stateHandle + ")"), fun))
    }

    // Numeric state helpers inc, dec, addTo - these methods do appropriate math operations on Numeric states.

    def inc[S: Numeric](stateHandle: StateHandle[S], name: String = "") {
      //(implicit ev: S <:< Numeric[S], n : Numeric[S])
      val n = implicitly[Numeric[S]]
      sb.addComponent(new StateUpdate[S, T](c, stateHandle,
        sb.nextLabel(name, "inc(" + stateHandle + ")"), (s, _) => n.plus(s, n.one)))
    }

    def dec[S: Numeric](stateHandle: StateHandle[S], name: String = "") {
      val n = implicitly[Numeric[S]]
      sb.addComponent(new StateUpdate[S, T](c, stateHandle,
        sb.nextLabel(name, "dec(" + stateHandle + ")"), (s, _) => n.minus(s, n.one)))
    }

    def addTo[S](stateHandle: StateHandle[S], name: String = "")(implicit n: Numeric[S], ev: T <:< S) {
      sb.addComponent(new StateUpdate[S, T](c, stateHandle,
        sb.nextLabel(name, "addTo(" + stateHandle + ")"),
        (s, a) => n.plus(s, a)
      ))
    }

    def withState[S](stateHandle: StateHandle[S]) = new ContactWithState[T, S](c, stateHandle)(sb)

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
      new LinkBuilderOps(c, devNull)(sb).stateFlatMap(
        stateHandle, sb.nextLabel(name, "" + stateHandle + " := setState(" + fun + ")")) {
        (s: S, t: T) ⇒ (fun(t), Seq())
      }
    }

    def resetState[S](stateHandle: StateHandle[S], name: String = "") =
      setState(stateHandle, sb.nextLabel(name, "" + stateHandle + " := s0"))(_ => stateHandle.s0)


    def passByStateCondition[S](stateHandle: StateHandle[S], name: String = "")(condition: S => Boolean): Contact[T] = {
      val res = sb.auxContact[T]
      (c -> res).stateFlatMap(stateHandle, sb.nextLabel(name, "pass if condition on " + stateHandle.name))((s, t) => if (condition(s)) (s, Seq(t)) else (s, Seq()))
      res
    }

    def passByStateConditionAndUpdateState[S](stateHandle: StateHandle[S], name: String = "")(condition: (S, T) => Option[S]): Contact[T] = {
      val res = sb.auxContact[T]
      (c -> res).stateFlatMap(stateHandle, sb.nextLabel(name, "pass if condition on " + stateHandle.name)) {
        (s, t) => val v = condition(s, t); if (v.isDefined) (v.get, Seq(t)) else (s, Seq())
      }
      res
    }

    def passIfEnabled(stateHandle: StateHandle[Boolean], name: String = "") =
      passByStateCondition(stateHandle, sb.nextLabel(name, "pass if " + stateHandle.name + "?"))(identity)

    def activate(stateHolder: StateHandle[Boolean], isActive: Boolean = true) = {
      labelNext("⇒" + isActive)
      val c2 = (c, sb.auxContact[Boolean]).map(_ ⇒ isActive)
      new ContactOps(c2)(sb).saveTo(stateHolder)
      c
    }

    def deactivate(stateHolder: StateHandle[Boolean]) = {
      activate(stateHolder, isActive = false)
      c
    }

    /** Extracts current state value. */
    def getState[S](stateHolder: StateHandle[S], name: String = ""): Contact[S] =
      (zipWithState(stateHolder) -> sb.auxContact[S]).map(_._1, sb.nextLabel(name, "_._1"))

    //    implicit
    //    def zippingLink[S](c: (Contact[T], Contact[(S, T)]))(sb: BasicSystemBuilder): ZippingLinkOps[S, T] = new ZippingLinkOps[S, T](c: (Contact[T], Contact[(S, T)]))(sb)

    /** Converts data to pair with current state value. */
    def zipWithState[S](stateHolder: StateHandle[S], name: String = ""): Contact[(S, T)] =
      new ZippingLinkOps(c -> sb.auxContact[(S, T)])(sb).zipWithState(stateHolder, name)

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

    /** Sets latch value it it has not been set yet */
    def latchValue[S >: T](stateHolder: StateHandle[Option[S]], f: T ⇒ S = locally[T](_)) = {
      new LinkBuilderOps(c, devNull)(sb).stateFlatMap(stateHolder, sb.nextLabel("", "" + stateHolder + "<?=Some")) {
        (s: Option[S], t: T) ⇒ (if (s.isEmpty) Some(f(t)) else s, Seq())
      }

    }


    /** fires fast execution until the given finishContacts. Be careful. */
    def fireUntilSet[T2 <: T](start: Contact[T2], finishContacts: Set[Contact[_]], name: String = "") {
      sb.addLink[T, T2](c, start, sb.nextLabel(name, "fire"),
        RedMapLink[T, T2](finishContacts + start))
    }

    /** fires fast execution until the given finishContacts. Be careful. */
    def fire[T2 <: T](start: Contact[T2], finishContacts: Contact[_]*) {
      sb.addLink[T, T2](c, start, sb.nextLabel("", "fire"),
        RedMapLink[T, T2](finishContacts.toSet + start))
    }


    def delayOne: Contact[T] =
      (c, sb.auxContact[T]).directly(sb.nextLabel("", "Δt"))

    /**
     * Create delay line that delays propagation of the signal by the given number of ticks.
     * For big counts there should be another implementation based on creating single special contact
     * and sending pair - (count, data) circulating until count == 0.
     * See #delayN and #countDown and #isZeroCase
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

    def delayN(count:Int):Contact[T] = {
      require(count >=2, "#delayN can only be used for count >= 2")
      val i = c.map(d => (count - 2, d))
      i.countDown().isZeroCase()
    }
    /** Calculates trellis positions of the given contacts using available links
      * and creates a delayed contact that will get the data from this contact `c` simultaneously
      * with the contact `c2`.*/
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

    /** Analogous to foldLeft. Every input is mixed with state by function f.
      * The result is saved to the state and returned on the next contact.
      * */
    //  def foldLeft[S](stateHandle:StateHandle[S])(f:(S, T) => S) = {
    //  }
  }

  implicit class StateLinkBuilder2Ops[T1, T2, S](p: (ContactWithState[T1, S], Contact[T2]))(implicit sb: SystemBuilder) {

    def stateMap(f: (S, T1) ⇒ (S, T2), name: String = "") =
      sb.addLink(p._1.c1, p._2,
        sb.nextLabel(name, "sm"),
        new StatefulFlatMapLink[S, T1, T2](
          (s, t) => {
            val r = f(s, t)
            (r._1, Seq(r._2))
          }, p._1.stateHandle))

    def stateFlatMap(f: (S, T1) ⇒ (S, GenTraversableOnce[T2]), name: String = "") =
      sb.addLink(p._1.c1, p._2, sb.nextLabel(name, "sfm"),
        new StatefulFlatMapLink[S, T1, T2](f, p._1.stateHandle))
  }

  implicit class StateOps[S](s: StateHandle[S])(implicit sb: SystemBuilder) {
    def >>:(c: Contact[S]) = {
      new ContactOps(c)(sb).saveTo(s)
      c
    }
  }

}
