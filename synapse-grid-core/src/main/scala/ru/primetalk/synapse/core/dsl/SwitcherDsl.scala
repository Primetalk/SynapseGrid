package ru.primetalk.synapse.core.dsl

import ru.primetalk.synapse.core.components.Contact0

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * API for creating "switcher". A switcher is a special mini-builder for constructing
 * a set of case branches that are "sealed" - the last branch "else" will take all the rest signals.
 * @author zhizhelev, 05.04.15.
 */
trait SwitcherDsl extends SystemBuilderDsl{

  class SwitcherBuilder[T](c: Contact[T], name: String = "")(implicit sb: SystemBuilder) {
    val defaultId: String = name + "Else"
    val selectorName: String = sb.nextLabel(name, "selector")

    case class Condition(id: String, condition: T => Boolean)

    var completed = false
    val conditions: ListBuffer[Condition] = mutable.ListBuffer[Condition]()
    val endPoints: ListBuffer[Contact0] = mutable.ListBuffer[Contact0]()
    /** Special intermediate contact to start fast signal processing. */
    val fireSelector: Contact[(String, T)] = sb.auxContact[(String, T)]
    val selector: Contact[(String, T)] = sb.auxContact[(String, T)]

    def If(condition: T => Boolean, name: String = ""): Contact[T] = {
      ensureNotCompleted()
      require(conditions.isEmpty, "If can only be the first clause in switcher. Use ElseIf or Else on other branches.")
      ElseIf(condition, name)
    }

    private def sCase(id: String) = {
      val res = new ContactPairOps(selector)(sb).Case(id)
      endPoints += res
      res
    }

    def ElseIf(condition: T => Boolean, name: String = ""): Contact[T] = {
      ensureNotCompleted()
      val id = sb.nextLabel(name, "" + conditions.size)
      conditions += Condition(id, condition)
      sCase(id)
    }

    private def ensureNotCompleted(): Unit = require(!completed, "the switcher " + name + " is completed. (There should be one and only one Else in a switcher.)")

    def Else(name: String = ""): Contact[T] = {
      ensureNotCompleted()
      val id = sb.nextLabel(name, defaultId)// this name will be used in sCase
      val res = ElseIf(_ => true, id)
      completed = true
      compileSelector()
      res
    }

    private def compileSelector(): Unit = {
      completed = true
      val conditionsList = conditions.toSeq
      def f(value: T): (String, T) = {
        val id = conditionsList.find(_.condition(value)).map(_.id).getOrElse(defaultId)
        (id, value)
      }
      LinkBuilderOps(c -> fireSelector).map(f,selectorName)
      new ContactOps(fireSelector)(sb).fireUntilSet(selector, endPoints.toSet)
    }
  }
  implicit class SwitcherContactOps[T](val c: Contact[T])(implicit sb: SystemBuilder) {
    def switcher(name: String = "") =
      new SwitcherBuilder[T](c, name)(sb)
  }

}
