///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011                                   //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev
 *
 * Created: 11.03.2015 zhizhelev
 */
package ru.primetalk.synapse.core

import org.scalatest.FunSuite

class TypedSystemConstructorTest extends FunSuite {

  class MySystemInterface {
    val name = "MySystem"
    val i1 = contact[String]("i1")
    val i2 = contact[String]("i2")
    val o1 = contact[String]("o1")
  }
  implicit object MySystemImplementation extends TypedSystemConstructor[MySystemInterface]{
    override def apply(outer: MySystemInterface): StaticSystem = {
      import outer._
      implicit val sb = systemBuilderTyped(name)(i1,i2)(o1)
      i1 >> o1
      i2 >> o1
      sb.toStaticSystem
    }
  }

  // create a pure instance of the system's outer interface without any connection to SystemBuilder
  val s = new MySystemInterface

  test("copy"){
    val tr = new RichDynamicSystem(s.toStaticSystem.toDynamicSystem).toMapTransducer(s.i1, s.o1)
    for(i<-0 until 10)
      assert(tr(""+i) === ""+i)
  }

}
