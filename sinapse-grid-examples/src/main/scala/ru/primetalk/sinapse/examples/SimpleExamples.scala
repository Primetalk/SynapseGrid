///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SinapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 * (2-clause BSD license) See LICENSE
 *
 * Created: 28.06.13, zhizhelev
 */
package ru.primetalk.sinapse.examples

import ru.primetalk.sinapse.core._
import scala.util.matching.Regex

object SimpleExamples {

  val myContact = new Contact[String]("myContact")
  val len = new Contact[Int]("len")

  /** Example fixture */
  abstract class Example[TInput, TOutput](name: String) extends SystemBuilderC(name) {
    val input = contact[TInput]("input")
    val output = contact[TOutput]("output")

    inputs(input)
    outputs(output)

    def exampleInput[T2 >: TInput](c: Contact[T2]) = mappedInput(input, c)

    def exampleOutput[T2 <: TOutput](c: Contact[T2]) {
      mappedOutput(c, output)
    }

  }
  /** Example fixture */
  abstract class ExampleMap[TInput, TOutput](name: String) extends Example[TInput, TOutput](name) {
    lazy val exampleMapTransducer: TInput => TOutput  = {
      val s = toStaticSystem
      s.toDot.saveTo(s"target/${s.name}.dot")
      val transducer = s.toDynamicSystem.toMapTransducer(input, output)
      transducer(_)
    }
    def apply(data:TInput):TOutput = exampleMapTransducer(data)

  }
  /** Example fixture */
  abstract class ExampleFlatMap[TInput, TOutput](name: String) extends Example[TInput, TOutput](name) {

    lazy val exampleFlatMapTransducer: TInput => List[TOutput] = {
      val s = toStaticSystem
      s.toDot.saveTo(s"target/${s.name}.dot")
      val transducer = s.toDynamicSystem.toTransducer(input, output)
      transducer(_)
    }

    def apply(data:TInput):List[TOutput] = exampleFlatMapTransducer(data)

  }

  val e1 = new ExampleMap[String, Int]("example1") {
    val myContact = new Contact[String]("myContact")
    val len = new Contact[Int]("len")

    def getLength(s: String) = s.length

    myContact -> len map(getLength, "getLength")

    exampleInput(myContact)
    exampleOutput(len)
  }
  val e2 = new ExampleFlatMap[String, String]("example2") {
    val myContact = new Contact[String]("myContact")

    val spaces: Regex = "\\s+".r
    val wordsContact = myContact.flatMap(spaces.split(_), "spaces.split(_)")

    exampleInput(myContact)
    exampleOutput(wordsContact)
  }
  val e3 = new ExampleMap[String, Int]("example3") {
    val myContact = new Contact[String]("myContact")
    val counterS = state[Int]("counterS", 0)
    val helloCount = contact[Int]("helloCount")
    (myContact.withState(counterS) -> helloCount).stateMap({
      (counter: Int, any:String) => (counter + 1, counter + 1)
    },"inc "+counterS)
//    (myContact -> helloCount ).stateMap (counterS, "inc "+counterS)
//      {(counter: Int, any:String) => (counter + 1, counter + 1)}

    exampleInput(myContact)
    exampleOutput(helloCount)
  }

}
