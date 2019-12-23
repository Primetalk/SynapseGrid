package ru.primetalk.contacts.core

import UniSets._

import scala.annotation.tailrec

trait ComponentAlgebraDependentBase { base =>
  type ComponentShape[A<: UniSet, B<: UniSet] = (A,B)
  /** This is for user to implement/define.
    * User should create a component type-level identifier that
    * extends this type.
    */
  trait Component {
    // set of input contacts
    type In <: UniSet
    // set of output contacts
    type Out <: UniSet
  }

  /** One of the mechanisms to create new components is to put them in parallel. */
  sealed trait ParallelAdd[C1 <: Component, C2 <: Component] extends Component {
    type In = Union[C1#In, C2#In]
    type Out = Union[C1#Out, C2#Out]
    type Self = ParallelAdd[C1, C2]
  }

  def parallelAdd[C1 <: Component, C2 <: Component](c1: C1, c2: C2): ParallelAdd[C1, C2] =
    new ParallelAdd[C1, C2]{}

  /** A powerful mechanisms to compose components is to put them on the breadboard one by one.
    * and then at some moment produce a new component by projecting the breadboard on some inputs and outputs. */
  sealed trait Breadboard {
    type Sinks <: UniSet
    type Sources <: UniSet
  }

  object EmptyBreadboard extends Breadboard {
    type Sinks = Empty
    type Sources = Empty
    type Impl = base.ImplementationComponent[EmptyBreadboard]
    def withAddedComponent[C <: Component]: WithAddedComponent[C, EmptyBreadboard] =
      new WithAddedComponent[C, EmptyBreadboard] {}
    def toComponent[I <: UniSet, O <: UniSet](implicit i: IsSubSetOf[I, Empty]): ToComponent[I, O, EmptyBreadboard] =
      new ToComponent[I, O, EmptyBreadboard] {}

  }
  type EmptyBreadboard = EmptyBreadboard.type
  sealed trait WithAddedComponent[C <: Component, B <: Breadboard] extends Breadboard {
    type Sinks = Union[C#In, B#Sinks]
    type Sources = Union[C#Out, B#Sources]
    type Self = WithAddedComponent[C, B]
    type Impl = base.ImplementationComponent[Self]

    def withAddedComponent[C1 <: Component]: WithAddedComponent[C1, Self] =
      new WithAddedComponent[C1, Self] {}
    def toComponent[I1 <: UniSet, O1 <: UniSet](implicit i: IsSubSetOf[I1, Sinks]): ToComponent[I1, O1, Self] =
      new ToComponent[I1, O1, Self] {}

  }

  sealed trait ImplementationComponent[B <: Breadboard] extends Component {
    type In = B#Sinks
    type Out = B#Sources
    type Self = ImplementationComponent[B]
  }
  sealed trait ToComponent[I <: UniSet, O <: UniSet, B <: Breadboard] extends Component {
    type In = I
    type Out = O
    type Self = ToComponent[I, O, B]
  }

}

trait ComponentAlgebraDependentFeatures extends ComponentAlgebraDependentBase with Signals {

  sealed trait HandlerOf[C <: Component] {
    def handler: C#In >> C#Out
  }

}
trait HandlerOfsDependent extends ComponentAlgebraDependentFeatures {
  def defineHandlerOf[C <: Component](f: C#In >> C#Out): HandlerOf[C] = new HandlerOf[C] {
    override def handler: C#In >> C#Out = f
  }

  def convertHandlerOf[C1 <: Component, C2 <: Component]
  (implicit h: HandlerOf[C1], evin: IsSubSetOf[C2#In, C1#In],evout: IsSubSetOf[C1#Out, C2#Out]): HandlerOf[C2] = new HandlerOf[C2] {
    override def handler: C2#In >> C2#Out = s => {
      val s1 = s.cProjection[C1#In]
      val o1s = h.handler(s1)
      o1s.map(o1 => o1.cProjection[C2#Out])
    }
  }

  implicit def parallelAddHandlerOf[C1 <: Component, C2 <: Component]
  (implicit
   h1: HandlerOf[C1],
   h2: HandlerOf[C2],
   i1: Render[Contact, C1#In],
   i2: Render[Contact, C2#In],
   o: Render[Contact, Union[C1#Out, C2#Out]]
  ): HandlerOf[ParallelAdd[C1, C2]] =
    new HandlerOf[ParallelAdd[C1, C2]] {
      override def handler: Union[C1#In, C2#In] >> Union[C1#Out, C2#Out] = signal => {
        val s1 = signal.projection0[C1#In].toIterable
        val s2 = signal.projection0[C2#In].toIterable
        val out1: Iterable[Signal[C1#Out]] = s1.flatMap(a => h1.handler(a))
        val out2: Iterable[Signal[C2#Out]] = s2.flatMap(a => h2.handler(a))
        val res =
          out1.map(_.cProjection[Union[C1#Out, C2#Out]]) ++
            out2.map(_.cProjection[Union[C1#Out, C2#Out]])
        res
      }
    }

  implicit def emptyComponentHandlerOf[C<: Component{ type In = Empty; type Out = Empty }]: HandlerOf[C] =
    new HandlerOf[C] {
      override def handler: Empty >> Empty = s => throw new IllegalArgumentException(s"emptyComponentHandler.handler cannot get any input $s")
    }

  implicit def addComponentHandlerOf[B0 <: Breadboard, C <: Component]
  (implicit
   h1: HandlerOf[C],
   h2: HandlerOf[ImplementationComponent[B0]],
   i1: Render[Contact, C#In],
   i2: Render[Contact, B0#Sinks],
   o: Render[Contact, Union[C#Out, B0#Sources]]
  )
  :    HandlerOf[ImplementationComponent[WithAddedComponent[C, B0]]] =
  new HandlerOf[ImplementationComponent[WithAddedComponent[C, B0]]] {
          override def handler: Union[C#In, B0#Sinks] >> Union[C#Out, B0#Sources] = signal => {
            val s1 = signal.projection0[C#In].toIterable
            val s2 = signal.projection0[B0#Sinks].toIterable
            val out1: Iterable[Signal[C#Out]] = s1.flatMap(a => h1.handler(a))
            val out2: Iterable[Signal[B0#Sources]] = s2.flatMap(a => h2.handler(a))
            val res =
              out1.map(_.cProjection[Union[C#Out, B0#Sources]]) ++
                out2.map(_.cProjection[Union[C#Out, B0#Sources]])
            res
          }
        }

  implicit def toComponentHandlerOf[I <: UniSet, O <: UniSet, B <: Breadboard]
  (implicit
    bh: HandlerOf[ImplementationComponent[B]],
     inputs1: Render[Contact, I],
     outputs1: Render[Contact, O],
     i: IsSubSetOf[I, B#Sinks], //o: IsSubSetOf[O, Sources],
    nonOutputsIsSubsetOfInputs: IsSubSetOf[B#Sources, Union[B#Sinks, O]]
  )
  : HandlerOf[ToComponent[I, O, B]] = new HandlerOf[ToComponent[I, O, B]] {
    override def handler: I >> O = signal => {
      @tailrec
      def loop(innerInputSignals: Iterable[Signal[B#Sinks]], tempOutput: Iterable[Signal[O]]): Iterable[Signal[O]] = {
        if(innerInputSignals.isEmpty)
          tempOutput
        else {
          val innerResults: Iterable[Signal[B#Sources]] = innerInputSignals.flatMap(bh.handler)
          val sortedResults = innerResults.map{s => projection0EitherUnion[B#Sinks, O, B#Sources](s)}
          val lefts = sortedResults.flatMap(_.left.toOption)
          val rights = sortedResults.flatMap(_.toOption)
          val leftsAsInputs = lefts.map(_.cProjection[B#Sinks])
          loop(leftsAsInputs, tempOutput ++ rights)
        }
      }
      val inputSignal = signal.cProjection[B#Sinks](i)
      loop(Iterable.single(inputSignal), Iterable.empty)
    }
  }
}

trait ComponentAlgebraDependentDSL extends HandlerOfsDependent with MySignals { self =>


  sealed trait MyContact extends Contact

  abstract class ContactImpl[A](val name: String) extends Product with Serializable with MyContact {
    override type Data = A
  }

  /** It's a simple component that has one input and one output.
    * The component provides contacts inside itself.
    * This makes it easier to deal with.
    */
  sealed trait InOutComponent0  extends Component {
    type InContact <: MyContact
    type OutContact <: MyContact
    val inContact: InContact
    val outContact: OutContact
    type In = Singleton[InContact]
    type Out = Singleton[OutContact]
  }
  trait InOutComponent[DI, DO] extends InOutComponent0 {
    case object In extends ContactImpl[DI]("In")
    case object Out extends ContactImpl[DO]("Out")
    type InContact = In.type
    type OutContact = Out.type
    val inContact: InContact = In
    val outContact: OutContact = Out

  }

  case class LinkComponent[InC <: MyContact, OutC <: MyContact](inC: InC, outC: OutC) extends InOutComponent0 {
    type InContact = InC
    type OutContact = OutC
    lazy val inContact: InContact = inC
    lazy val outContact: OutContact = outC
  }

  object LinkComponent {
    /**
      * Handler is only available if the types of in/out are compatible.
      */
    implicit def handlerOfLinkComponent[InC <: MyContact:ValueOf, OutC <: MyContact:ValueOf](implicit ev: InC#Data <:< OutC#Data)
    : HandlerOf[LinkComponent[InC, OutC]] =
      new HandlerOf[LinkComponent[InC, OutC]] {
        override def handler: Si[InC] >> Si[OutC] = identity(valueOf[InC], valueOf[OutC])
      }
  }
  /**
    * This component connects two other in-out components.
    */
  trait LinkTwoInOutComponents[C1 <: InOutComponent0, C2 <: InOutComponent0] extends InOutComponent0 {
    val c1: C1
    val c2: C2
    type InContact = C1#OutContact
    type OutContact = C2#InContact
    lazy val inContact: InContact = c1.outContact
    lazy val outContact: OutContact = c2.inContact
  }
  // NB! Extension methods do not work well with dependent types.
  // When using `implicit class Ops[C](c: C)` there are two cases.
  // if we return `c.type` - compiler would say that it's a leak of a private val,
  // if instead we declare `val c: C`, that will create a new singleton-type.
  // Simple functions work correctly:
  def defineComponentIterable[C <: InOutComponent0](c: C)(f: c.InContact#Data => Iterable[c.OutContact#Data]): HandlerOf[c.type] =
    defineHandlerOf[c.type](self.liftIterable(c.inContact, c.outContact)(a => f(a)))
  def defineComponentImpl[C <: InOutComponent0](c: C)(f: c.InContact#Data => c.OutContact#Data): HandlerOf[c.type] =
    defineHandlerOf[c.type](self.lift(c.inContact, c.outContact)(a => f(a)))

}

trait ComponentAlgebraDependent extends ComponentAlgebraDependentDSL {

}
