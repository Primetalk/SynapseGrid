package ru.primetalk.synapse.core.subsystems

import ru.primetalk.synapse.core.ext.SystemBuilderApi

import scala.language.implicitConversions

/**
 * @author zhizhelev, 25.03.15.
 */
trait BaseTypedSystemDsl extends SystemBuilderApi {
  /** A system builder with inputs and outputs given in advance.
    * */
  def systemBuilderTyped(name:String)(_inputs:Contact[_]*)(_outputs:Contact[_]*):SystemBuilder = {
    val res = new SystemBuilderC(name)
    res.inputs(_inputs:_*)
    res.outputs(_outputs:_*)
    res
  }

  /** Implements a system of type T.
    * Uses external SystemBuilder instance to define internal System's structure.*/
  trait SystemImplementation[T]{
    def apply(outer:T)(implicit sb:SystemBuilder):Unit
  }
  /**
   * Another way for system's construction is to define inputs and outputs in a separate class/trait
   * and then enumerate them in system builder: sb.inputs(in1, in2, in3) and sb.outputs(out1, out2)
   *
   * class MySystemsInterface {
   * val in1 = new Contact[String]("in1")
   * val in2 = new Contact[String]("in2")
   * val out1 = new Contact[String]("out1")
   * }
   *
   * implicit object MySystemImplementation extends TypedSystemConstructor[T] {
   * def apply(outer:T):StaticSystem = {
   *
   * }
   * }
   */
  trait TypedSystemConstructor[T] extends (T => StaticSystem) {
    def apply(outer: T): StaticSystem
  }
  /** */
  case class TypedSystem[T](outer:T, staticSystem: StaticSystem) extends WithStaticSystem {
    override def toStaticSystem: StaticSystem = staticSystem
  }

  //  case class TypedSystem2[T](name:String)(implicit outerBuilder:OuterInterfaceBuilder => T, definitionBuilder:SystemImplementation[T]) extends WithStaticSystem {
//    val (outer, toStaticSystem) = {
//      val sb = new SystemBuilderC(name)
//      val outer = outerBuilder(sb)
//      definitionBuilder.apply(outer)(sb)
//      (outer, sb.toStaticSystem)
//    }
//  }
  def createTypedSystem[T](name:String)(implicit outerBuilder:OuterInterfaceBuilder => T, systemImplementation:SystemImplementation[T]):TypedSystem[T] = {
    val sb = new SystemBuilderC(name)
    val outer = outerBuilder(sb)
    systemImplementation.apply(outer)(sb)
    TypedSystem(outer, sb.toStaticSystem)
  }
//  (outerBuilder:OuterInterfaceBuilder => T) extends WithStaticSystem {
//    implicit val sb:SystemBuilder = new SystemBuilderC()
//    val outer = outerBuilder(sb)
//    def toStaticSystem = sb.toStaticSystem
//    sb.addSubsystem()
//  }

}
