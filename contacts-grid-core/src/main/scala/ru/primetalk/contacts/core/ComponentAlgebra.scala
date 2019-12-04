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
    A <: Component[I1, O1], B<: Component[I2, O2]] extends Component[Union[I1, I2], Union[O1, O2]]

  def parallelAdd[I1 <: UniSet, O1 <: UniSet, I2 <: UniSet, O2 <: UniSet,
    A <: Component[I1, O1], B<: Component[I2, O2]](a: A, b: B): ParallelAdd[I1, O1, I2, O2, A, B] =
    new ParallelAdd[I1, O1, I2, O2, A, B]{}

  /** A powerful mechanisms to compose components is to put them on the breadboard one by one.
    * and then at some moment produce a new component by projecting the breadboard on some inputs and outputs. */
  sealed trait Breadboard[Sinks <: UniSet, Sources <: UniSet] {
    sealed trait ToComponent[I <: UniSet, O <: UniSet] extends Component[I, O]
    def toComponent[I <: UniSet, O <: UniSet]: ToComponent[I, O] = new ToComponent[I, O] {}
    sealed trait AddComponent[I <: UniSet, O <: UniSet] extends Breadboard[Union[I, Sinks], Union[O, Sources]]
    def addComponent[I <: UniSet, O <: UniSet]: AddComponent[I, O] = new AddComponent[I, O] {}
  }
}

trait ComponentAlgebraFeatures extends ComponentAlgebraBase {
//
//  trait HandlerOf[Shape <: ComponentShape, C <: Component[Shape]] {
//    def handler: Shape#InputShape >> Shape#OutputShape
//  }
//
//  def defineHandler[Shape <: ComponentShape, C <: Component[Shape]](f: Shape#InputShape >> Shape#OutputShape): HandlerOf[Shape, C] = new HandlerOf[Shape, C] {
//    override def handler: Shape#InputShape >> Shape#OutputShape = f
//  }
}
trait HandlerOfs extends ComponentAlgebraBase {

}