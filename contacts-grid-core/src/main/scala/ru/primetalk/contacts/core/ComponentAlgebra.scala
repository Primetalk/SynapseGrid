package ru.primetalk.contacts.core

import UniSets._

trait ComponentAlgebraBase {
  type ComponentShape[A<: UniSet, B<: UniSet] = (A,B)
  sealed trait Breadboard
  trait Lift[S] extends Breadboard
  trait ParallelAdd[A <: Breadboard, B<: Breadboard] extends Breadboard
  trait SequentialAdd[A <: Breadboard, B<: Breadboard] extends Breadboard
  trait Projection[A <: Breadboard, S]
  trait ToComponent[B]
//  trait Breadboard[....] extends Breadboard
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