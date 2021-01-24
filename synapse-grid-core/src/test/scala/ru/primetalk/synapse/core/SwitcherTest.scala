package ru.primetalk.synapse.core

import org.junit.Test
import syntax._
import syntax.given

/**
 * @author zhizhelev, 24.08.15.
 */
class SwitcherTest {
  object SwitcherSystem extends BaseTypedSystem {
    val level = input[Int]("level")
    val highLevel = output[Int]("highLevel")
    val mediumLevel = output[Int]("mediumLevel")
    val lowLevel = output[Int]("lowLevel")

    override protected def defineSystem(implicit sb: SystemBuilder): Unit = {
      val sw    = level.switcher("sw")

      sw.If(_ > 10, "_ > 10")   >> highLevel
        sw.ElseIf(_ > 3, "_ > 3") >> mediumLevel
        sw.Else("else")           >> lowLevel

    }
  }

  @Test def switcher(): Unit =
    SwitcherSystem.toStaticSystem.toDot().saveTo("SwitcherSystem.dot")
}