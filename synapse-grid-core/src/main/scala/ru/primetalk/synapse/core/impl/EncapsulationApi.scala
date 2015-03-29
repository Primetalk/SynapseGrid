package ru.primetalk.synapse.core.impl

import ru.primetalk.synapse.core._

/**
 * @author zhizhelev, 29.03.15.
 */
trait EncapsulationApi {

  /** An interface of some object that can collect information about outer interface of a system.
    * Not only it create contact instances, but it usually transforms and collect them. */
  trait OuterInterfaceBuilder {
    /** creates a contact that will be used as input contact. */
    def input[T](internalName: String): Contact[T]

    /** creates a contact that will be used as output contact. */
    def output[T](internalName: String): Contact[T]
  }

  object SimpleOuterInterfaceBuilder extends OuterInterfaceBuilder {
    override def input[T](internalName: String): Contact[T] = new Contact[T](internalName)

    override def output[T](internalName: String): Contact[T] = new Contact[T](internalName)
  }

  class EmbeddedOuterInterfaceBuilder(name:String)(implicit sb:SystemBuilder) extends OuterInterfaceBuilder{
    override def input[T](internalName: String): Contact[T] = sb.input(name + "." + internalName)

    override def output[T](internalName: String): Contact[T] = sb.output(name + "." + internalName)
  }
  /** Usage: see defineEncapsulation*/
  abstract class EncapsulationBuilder[Outer](name:String)(outerDefinition:OuterInterfaceBuilder=>Outer) {
    protected
    implicit val sb = new SystemBuilderC(name)

    val outer = outerDefinition(new EmbeddedOuterInterfaceBuilder(name)(sb))

    def toStaticSystem = sb.toStaticSystem
  }
  /** Usage:
    * class OuterInterface(b:OuterInterfaceBuilder){
    *   def in1 = b.input[Int]("in1")
    *   def out1 = b.output[Int]("out1")
    * }
    * class OuterImplementation(name:String) extends EncapsulationBuilder(name)(new OuterInterface){
    *   in1 >> out1
    * }
    * defineEncapsulation(new OuterImplementation("mySystem1"))
    * */
  def defineEncapsulation[Outer](en:EncapsulationBuilder[Outer])(implicit sb:SystemBuilder):Outer = {
    sb.addSubsystem(en.toStaticSystem)
    en.outer
  }
//  implicit class EncapsulationBuilderE(sb: BasicSystemBuilder) {
//    /** Creates both outer interface of a system and internal implementation.
//      *
//      */
//    def defineEncapsulatedSystem[S](name: String)(f: OuterInterfaceBuilder => S)(body: S => StaticSystem) = {
//      val innerSb = new SystemBuilderC(name)
//      val system = f(new EmbeddedOuterInterfaceBuilder(name)(innerSb))
//      sb.addSubsystem(system)
//    }
//  }

}
