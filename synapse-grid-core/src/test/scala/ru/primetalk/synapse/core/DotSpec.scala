package ru.primetalk.synapse.core

import org.specs2._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import scala.language.implicitConversions

/**
 * @author zhizhelev, 19.04.15.
 */
@RunWith(classOf[JUnitRunner])
class DotSpec extends Specification { def is = s2"""

  StaticSystem.toDot should
    produce some output     ${s1dot mustNotEqual ""}
    contain system's name   ${s1dot must contain("S1")}
    contain link label >>   ${s1dot must contain(">>")}
            """

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

}
