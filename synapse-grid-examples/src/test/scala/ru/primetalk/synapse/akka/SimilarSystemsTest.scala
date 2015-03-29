package ru.primetalk.synapse.akka

import org.scalatest.FunSuite
import ru.primetalk.synapse.core._

/**
 * The test checks that a few identical child systems work seamlessly together. In particular
 * one can send a signal to a selected system.
 * @author zhizhelev, 26.03.15.
 */
class SimilarSystemsTest extends FunSuite {
  class Child(b: OuterInterfaceBuilder) {
    val i1:Contact[Int] = b.input("i1")
    val o1:Contact[Boolean] = b.output("o1")
  }
//  class Child(val index:Int) extends ChildT {
//    val name = "Child"
//    val input = new Contact[Int]("input")
//    val output = new Contact[Boolean]("output")
//  }
  class ChildImplementation(index: Int) extends EncapsulationBuilder[Child]("child" + index)(new Child(_)) {
    outer.i1.filter(_ == index).const(true) >> outer.o1
  }

  class Parent(b: OuterInterfaceBuilder) {
    val input = b.input[Int]("input")
    val containsChild = b.output[Boolean]("containsChild")
  }
  class ParentImplementation extends EncapsulationBuilder[Parent]("Parent")(new Parent(_)){
      import outer._
      val children = (0 until 10).map{ i =>
        val child = defineEncapsulation(new ChildImplementation(i))
        input >> child.i1
        child.o1 >> containsChild
        child
      }
      sb.toStaticSystem
  }

  test("contains child"){
    val impl = new ParentImplementation
    val tr = impl.toStaticSystem.toDynamicSystem.toMapTransducer(impl.outer.input, impl.outer.containsChild)
    for(i<-0 until 10)
      assert(tr(i))
//    for(i <- 10 until 20)
//      assert(!tr(i))
  }


}
