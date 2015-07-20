package ru.primetalk.synapse.core.dsl

import scala.collection.mutable

/**
 * API for creating "switcher". A switcher is a special mini-builder for constructing
 * a set of case branches that are "sealed" - the last branch "else" will take all the rest signals.
 * @author zhizhelev, 05.04.15.
 */
trait SwitcherDsl extends SystemBuilderDsl{

  class SwitcherBuilder[T](c: Contact[T], name: String = "")(implicit sb: SystemBuilder) {
    val defaultId = name + "Else"
    val selectorName = sb.nextLabel(name, "selector")

    case class Condition(id: String, condition: T => Boolean)

    var completed = false
    val conditions = mutable.ListBuffer[Condition]()
    val endPoints = mutable.ListBuffer[Contact[_]]()
    val selector = sb.auxContact[(String, T)]

    def If(condition: T => Boolean, name: String = "") = {
      require(conditions.size == 0, "If can only be the first clause in switcher. Use ElseIf or Else on other branches.")
      ElseIf(condition, name)
    }

    private def sCase(id: String) = {
      val res = new ContactPairOps(selector)(sb).Case(id)
      endPoints += res
      res
    }

    def ElseIf(condition: T => Boolean, name: String = "") = {
      require(!completed, "the switcher " + name + " is completed.")
      val id = sb.nextLabel(name, "" + conditions.size)
      conditions += Condition(id, condition)
      sCase(id)
    }

    def Else(name: String = "") = {
      require(!completed, "the switcher " + name + " is completed.")
      completed = true
      compileSelector()
      sCase(defaultId)
    }

//    implicit def implLinkBuilder[T1, T2](c: (Contact[T1], Contact[T2])): LinkBuilderOps[T1, T2] = new LinkBuilderOps(c)(sb)
//
    private def compileSelector() {
      completed = true
      val conditionsList = conditions.toList
      val preSelector = sb.auxContact[(String, T)]
      (c -> preSelector).map(value => {
        val id = conditionsList.find(_.condition(value)).map(_.id).getOrElse(defaultId)
        (id, value)
      }, selectorName)
      new ContactOps(preSelector)(sb).fireUntilSet(selector, endPoints.toSet)
    }
  }
  implicit class SwitcherContactOps[T](val c: Contact[T])(implicit sb: SystemBuilder) {
    def switcher(name: String = "") =
      new SwitcherBuilder[T](c, name)(sb)
  }

}
