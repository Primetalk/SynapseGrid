package ru.primetalk.contacts.core

import UniSets._

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
  sealed trait ParallelAdd[I1 <: UniSet, O1 <: UniSet, I2 <: UniSet, O2 <: UniSet,
    C1 <: Component[I1, O1], C2 <: Component[I2, O2]] extends Component[Union[I1, I2], Union[O1, O2]]

  def parallelAdd[I1 <: UniSet, O1 <: UniSet, I2 <: UniSet, O2 <: UniSet,
    C1 <: Component[I1, O1], C2 <: Component[I2, O2]](c1: C1, c2: C2): ParallelAdd[I1, O1, I2, O2, C1, C2] =
    new ParallelAdd[I1, O1, I2, O2, C1, C2]{}

  /** A powerful mechanisms to compose components is to put them on the breadboard one by one.
    * and then at some moment produce a new component by projecting the breadboard on some inputs and outputs. */
  sealed trait Breadboard[Sinks <: UniSet, Sources <: UniSet] {
    sealed trait ToComponent[I <: UniSet, O <: UniSet] extends Component[I, O]
    def toComponent[I <: UniSet, O <: UniSet]: ToComponent[I, O] = new ToComponent[I, O] {}
    sealed trait AddComponent[I <: UniSet, O <: UniSet] extends Breadboard[Union[I, Sinks], Union[O, Sources]]
    def addComponent[I <: UniSet, O <: UniSet]: AddComponent[I, O] = new AddComponent[I, O] {}
  }
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

  implicit def parallelAddHandlerOf[I1 <: UniSet, O1 <: UniSet, I2 <: UniSet, O2 <: UniSet,
    C1 <: Component[I1, O1], C2 <: Component[I2, O2]]
  (implicit h1: HandlerOf[I1, O1, C1], h2: HandlerOf[I2, O2, C2],
   i1: Render[Contact, I1],
   i2: Render[Contact, I2],
   o: Render[Contact, Union[O1, O2]]
  ): HandlerOf[Union[I1, I2], Union[O1, O2], ParallelAdd[I1, O1, I2, O2, C1, C2]] =
    new HandlerOf[Union[I1, I2], Union[O1, O2], ParallelAdd[I1, O1, I2, O2, C1, C2]] {
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
}