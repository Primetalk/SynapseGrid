package ru.primetalk.contacts.core

import UniSets._

import scala.annotation.tailrec

trait ComponentAlgebraBase {
  type ComponentShape[A<: UniSet, B<: UniSet] = (A,B)
  /** This is for user to implement/define.
    * User should create a component type-level identifier that
    * extends this type.
    * @tparam I - set of input contacts
    * @tparam O - set of output contacts
    */
  trait Component[I <: UniSet, O <: UniSet]

  /** One of the mechanisms to create new components is to put them in parallel. */
  sealed trait ParallelAdd[I1 <: UniSet, O1 <: UniSet, C1 <: Component[I1, O1],
    I2 <: UniSet, O2 <: UniSet, C2 <: Component[I2, O2]] extends Component[Union[I1, I2], Union[O1, O2]]

  def parallelAdd[I1 <: UniSet, O1 <: UniSet,  C1 <: Component[I1, O1],
    I2 <: UniSet, O2 <: UniSet, C2 <: Component[I2, O2]](c1: C1, c2: C2): ParallelAdd[I1, O1, C1, I2, O2, C2] =
    new ParallelAdd[I1, O1, C1, I2, O2, C2]{}

  /** A powerful mechanisms to compose components is to put them on the breadboard one by one.
    * and then at some moment produce a new component by projecting the breadboard on some inputs and outputs. */
  sealed trait Breadboard[Sinks <: UniSet, Sources <: UniSet] {
    type Sinks0 = Sinks
    type Sources0 = Sources
    sealed trait ImplementationComponent extends Component[Sinks, Sources]
    sealed trait ToComponent[I <: UniSet, O <: UniSet] extends Component[I, O]
    def toComponent[I <: UniSet, O <: UniSet]: ToComponent[I, O] =
      new ToComponent[I, O] {}
    sealed trait WithAddedComponent[I <: UniSet, O <: UniSet, C <: Component[I, O]] extends Breadboard[Union[I, Sinks], Union[O, Sources]]
    def withAddedComponent[I <: UniSet, O <: UniSet, C <: Component[I, O]]: WithAddedComponent[I, O, C] =
      new WithAddedComponent[I, O, C] {}
  }

  object EmptyBreadboard extends Breadboard[Empty, Empty]
}

trait ComponentAlgebraFeatures extends ComponentAlgebraBase with Signals {

  sealed trait HandlerOf[I <: UniSet, O <: UniSet, C <: Component[I, O]] {
    def handler: I >> O
  }

}
trait HandlerOfs extends ComponentAlgebraFeatures {
  def defineHandlerOf[I <: UniSet, O <: UniSet, C <: Component[I, O]](f: I >> O): HandlerOf[I, O, C] = new HandlerOf[I, O, C] {
    override def handler: I >> O = f
  }

  def convertHandlerOf[I <: UniSet, O <: UniSet, C1 <: Component[I, O], C2 <: Component[I, O]]
  (implicit h: HandlerOf[I, O, C1]): HandlerOf[I, O, C2] = new HandlerOf[I, O, C2] {
    override def handler: I >> O = h.handler
  }

  implicit def parallelAddHandlerOf[
    I1 <: UniSet, O1 <: UniSet, C1 <: Component[I1, O1],
    I2 <: UniSet, O2 <: UniSet, C2 <: Component[I2, O2]]
  (implicit h1: HandlerOf[I1, O1, C1], h2: HandlerOf[I2, O2, C2],
   i1: Render[Contact, I1],
   i2: Render[Contact, I2],
   o: Render[Contact, Union[O1, O2]]
  ): HandlerOf[Union[I1, I2], Union[O1, O2], ParallelAdd[I1, O1, C1, I2, O2, C2]] =
    new HandlerOf[Union[I1, I2], Union[O1, O2], ParallelAdd[I1, O1, C1, I2, O2, C2]] {
      override def handler: Union[I1, I2] >> Union[O1, O2] = signal => {
        val s1 = signal.projection0[I1].toIterable
        val s2 = signal.projection0[I2].toIterable
        val out1: Iterable[Signal[O1]] = s1.flatMap(a => h1.handler(a))
        val out2: Iterable[Signal[O2]] = s2.flatMap(a => h2.handler(a))
        val res =
          out1.map(_.cProjection[Union[O1, O2]]) ++
            out2.map(_.cProjection[Union[O1, O2]])
        res
      }
    }

  implicit def emptyComponentHandlerOf[C<: Component[Empty, Empty]]: HandlerOf[Empty, Empty, C] =
    new HandlerOf[Empty, Empty, C] {
      override def handler: Empty >> Empty = s => throw new IllegalArgumentException(s"emptyComponentHandler.handler cannot get any input $s")
    }

  implicit def addComponentHandlerOf[Sinks <: UniSet, Sources <: UniSet, B <: Breadboard[Sinks, Sources],
    I <: UniSet, O <: UniSet, C <: Component[I, O]]
  (implicit
   ph: HandlerOf[Union[I, Sinks], Union[O, Sources], ParallelAdd[I, O, C, Sinks, Sources, B#ImplementationComponent]]
  )
  : HandlerOf[Union[I, Sinks], Union[O, Sources], Breadboard[Union[I, Sinks], Union[O, Sources]]#ImplementationComponent] =
    convertHandlerOf[Union[I, Sinks], Union[O, Sources], ParallelAdd[I, O, C, Sinks, Sources, B#ImplementationComponent],
      Breadboard[Union[I, Sinks], Union[O, Sources]]#ImplementationComponent]

  implicit def toComponentHandlerOf[Sinks <: UniSet, Sources <: UniSet, B <: Breadboard[Sinks, Sources],  I <: UniSet, O <: UniSet]
  (implicit
  bh: HandlerOf[Sinks, Sources, B#ImplementationComponent],
   inputs1: Render[Contact, I],
   outputs1: Render[Contact, O],
   i: IsSubSetOf[I, Sinks], o: IsSubSetOf[O, Sources],
   nonOutputsIsSubsetOfInputs: IsSubSetOf[Sources, Union[Sinks, O]]
  )
  : HandlerOf[I, O, B#ToComponent[I, O]] = new HandlerOf[I, O, B#ToComponent[I, O]] {
    override def handler: I >> O = signal => {
      @tailrec
      def loop(innerInputSignals: Iterable[Signal[Sinks]], tempOutput: Iterable[Signal[O]]): Iterable[Signal[O]] = {
        if(innerInputSignals.isEmpty)
          tempOutput
        else {
          val innerResults = innerInputSignals.flatMap(bh.handler)
//          val renderer = implicitly[Render[Contact, O]]
          val sortedResults = innerResults.map{s => projection0EitherUnion[Sinks, O, Sources](s)}
          val lefts = sortedResults.flatMap(_.left.toOption)
          val rights = sortedResults.flatMap(_.toOption)
          val leftsAsInputs = lefts.map(_.cProjection[Sinks])
          loop(leftsAsInputs, tempOutput ++ rights)
        }
      }
      val inputSignal = signal.cProjection[Sinks](i)
      loop(Iterable.single(inputSignal), Iterable.empty)
    }
  }
}

trait ComponentAlgebraDSL extends HandlerOfs with MySignals { self =>

  class forComponentImpl[In <: Contact, Out <: Contact, C <: Component[Singleton[In], Singleton[Out]]](in: In, out: Out, c: C) {
    def liftIterable[A >: In#Data, B <: Out#Data](f: A => Iterable[B]): HandlerOf[Singleton[In], Singleton[Out], C] =
      defineHandlerOf[Singleton[In], Singleton[Out], C](self.liftIterable(in, out)(a => f(a)))
    def lift[A >: In#Data, B <: Out#Data](f: A => B): HandlerOf[Singleton[In], Singleton[Out], C] =
      defineHandlerOf[Singleton[In], Singleton[Out], C](self.lift(in, out)(a => f(a)))
  }

  def forComponent[In <: Contact, Out <: Contact, C <: Component[Singleton[In], Singleton[Out]]](in: In, out: Out, c: C): forComponentImpl[In, Out, C] =
    new forComponentImpl[In, Out, C](in, out, c)
}

trait ComponentAlgebra extends ComponentAlgebraDSL {

}
