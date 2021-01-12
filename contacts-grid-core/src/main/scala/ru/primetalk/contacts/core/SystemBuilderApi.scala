package ru.primetalk.contacts.core
/*
trait ContactTypes {
  type In0
  type In[A]<:In0
  type Out0
  type Out[A]<:Out0
  type Contact0 <: In0 with Out0
  type Contact[A] <: Contact0 with In[A] with Out[A]
}

trait ContactsDsl0 extends ContactTypes {
  def contact[T](name: String): Contact[T]
}

//import shapeless._
// TODO: HList ?
trait Shapes extends TypeSets {
//  trait Signal[C]
  // 1. нам надо свидетельство, что контакт входит в Shape. (и что не входит - тоже надо.
//  trait SignalBelongsToShape[Signal, Shape]
//  type <::<[Signal, Shape] = SignalBelongsToShape[Signal, Shape]


  trait NoImpl[T]
  type ¬[T] = T => Nothing
  type !![T] = NoImpl[T]
  object NoImpl {
    implicit def noimpl[T](implicit ev: T = null)
  }
  type <:<[Contact, Shape] = ContactBelongsToShape[Contact, Shape]
  def contactBelongsToShape[C,S](implicit s: ContactBelongsToShape[C, S]): Boolean = true
  // case class SignalShape[C](signal: Signal[C]) extends AnyVal with Shape
  type InputShape
  type OutputShape
  trait MatchingShapes[Outputs, Inputs]
  type ComponentShape[Inputs, Outputs]

}
// A few operations that can be done with a signal.
trait ShapedSignalAlgebra[ShapedSignal[_], Contact[_]] {
  def wrap[A, S <: Contact[A]](contactId: S, data: A): ShapedSignal[S]
  def unwrap[A, S <: Contact[A]](s: ShapedSignal[S]): (S, A)
  def getData[A, C <: Contact[A], S<:C](s: ShapedSignal[S], c: C): A
}

// contact is a component that has one input and one output,
// known transformation function = identity, and it works with 0 time.
trait SystemBuilderApi extends ContactsDsl0 with Shapes {

  type InputContactId [C, A] //= C => In[A]
  type OutputContactId[C, A] //= C => Out[A]

  // signal, specific to a contact `S`
  type ShapedSignal[S]

  type ShapedSignalProcessor[InShape, OutShape] = ShapedSignal[InShape] => Iterable[ShapedSignal[OutShape]]

  trait ComponentAlgebra[C] {
    type InShape
    type OutShape
    def toSignalProcessor(c: C): ShapedSignalProcessor[InShape, OutShape]
    type ComponentType = ShapedSignalProcessor[InShape, OutShape]

  }
  //  trait ShapedSignalWrapper[C] {
  //}
//  trait ShapedSignalUnwrapper[C] {
//    def unwrap[A](s: OutSignal[A]): (OutputContactId[C, A], A)
//  }

  class Function1Wrapper[A,B](f: A => B)(implicit sf: ShapedSignalAlgebra[ShapedSignal, Contact]) {
    object In extends Contact[A]
    object Out extends Contact[B]
    type ComponentType = ShapedSignalProcessor[In.type, Out.type]
    object Component extends ComponentType {
      override def apply(v1: ShapedSignal[In.type]): Iterable[ShapedSignal[Out.type]] =
        Iterable.single(sf.wrap(Out, f(sf.getData(v1, In))))
    }

  }

  implicit class Function1Algebra[A,B](implicit sf: ShapedSignalAlgebra[ShapedSignal, Contact]) extends ComponentAlgebra[A => B] {
    object In extends Contact[A]
    object Out extends Contact[B]
    override type InShape = In.type
    override type OutShape = Out.type
    override def toSignalProcessor(f: A => B): ShapedSignalProcessor[InShape, OutShape] = v1 =>
      Iterable.single(sf.wrap(Out, f(sf.getData(v1, In))))
  }
  class Function1EitherAlgebra[A,B,C](implicit sf: ShapedSignalAlgebra[ShapedSignal, Contact]) extends ComponentAlgebra[A => Either[A, B]] {
    object In extends Contact[A]
    object OutB extends Contact[B]
    object OutC extends Contact[C]
    override type InShape = In.type
    override type OutShape = OutB.type with OutC.type

    override def toSignalProcessor(f: A => Either[A,B]): ShapedSignalProcessor[InShape, OutShape] = v1 =>
      Iterable.single(
        f(sf.getData(v1, In)) match {
          case Left(l) => sf.wrap(OutB, l)
          case Right(r) => sf.wrap(OutC, r)
        }
      )
  }
  type Signal0
  type InSignal0 <: Signal0
  type OutSignal0 <: Signal0
  // Input signal
  type InSignal[A]<: InSignal0
  type OutSignal[A]<: OutSignal0

  type Signal[A] <: Signal0

  trait SignalWrapper[C] {
    def wrap[A](contactId: InputContactId[C, A], data: A): InSignal[A]
  }
  trait SignalUnwrapper[C] {
    def unwrap[A](s: OutSignal[A]): (OutputContactId[C, A], A)
  }

//
//  case class StatelessComponent[C](c: C, interpreter: SignalProcessor)
//
//  trait StatelessSignalInterpreter[Component] {
//    def statelessSignalProcessor(c: Component): SignalProcessor
//  }
//
//
//  type ComponentName[C]
//
//  trait HasInput[A] {
//    def input[A]: In[A]
//  }
//  trait HasOutput[A] {
//    def output[A]: Out[A]
//  }
//
//  sealed trait Link[A, B]
//  type LinkMap[A, B] <: Link[A,B]
//  def linkMap[A, B](f: A => B): LinkMap[A, B]
//  implicit class Function1StatelessSignalInterpreter[A, B](f: A => B) extends StatelessSignalInterpreter[A => B] {
//    def statelessSignalProcessor(f: A => B): SignalProcessor = ???
//  }
//
//  type Binding11[A,B]
//  type Solder[A] = (Out[A], In[A])
//  // signals that appear on `o` will be instantly converted to signals on `i`
//  def solder[A](o: Out[A], i: In[A]): Solder[A]
//  def connect[A](o: Out[A], i: In[A]): Solder[A] = solder(o,i)
//
//  def bind11[A,B](a: Out[A], b: In[B], l: Link[A,B]): Binding11[A,B]
//  implicit class OutOps[A](out: Out[A]) {
//    def map[B](f: A => B): Out[B] = ???
//  }
}

*/