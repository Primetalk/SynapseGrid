package ru.primetalk.synapse.core

import org.junit.Test
import syntax._
import syntax.given

/**
 * @author zhizhelev, 19.04.15.
 */
class DotTest { 
  
  implicit class S1(oib:OuterInterfaceBuilder) {
    import oib._
    val i1 = input[Int]("i1")
    val o1 = output[Int]("o1")
  }
  implicit object S1Impl extends SystemImplementation[S1]{
    override def apply(outer: S1)(implicit sb: SystemBuilder): Unit = {
      outer.i1 >> outer.o1
    }
  }
  val s1 = createTypedSystem[S1]("S1")

  val s1dot = s1.toDot()///new RichStaticSystemType(s1)(withStaticSystemToStaticSystem).toDot()
  @Test def `contain system's name`(): Unit =
    assert(s1dot.contains("S1"))
  @Test def `contain link label >>`(): Unit =
    assert(s1dot.contains(">>"))

}
