///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev
 *
 * Created: 08.12.13, zhizhelev
 */
package ru.primetalk.synapse
package examples

import core._
import rx._

/** An example of subsystems usage. */
object Example4App extends App {

  /** A subsystem that returns the length of a string and splits it into words.*/
  class StringStat extends BaseTypedSystem {
    import sb._
    val stringInput = input [String]("stringInput")
    val lenOutput   = output[Int]   ("lenOutput")
    val wordsOutput = output[String]("wordsOutput")
    (stringInput -> lenOutput).map(_.length, "len")
    (stringInput -> wordsOutput).flatMap(_.split(' '), "split")
    stringInput.foreach(n => println("="+n))
  }

  class WordStat extends BaseTypedSystem {
    import sb._
    val wordInput = input [String]("wordInput")
    val lenOutput = output[Int]   ("lenOutput")
    wordInput.
      filter(_.startsWith("H"), "_.startsWith('H')?").
      map(_.length, "len") >> lenOutput
  }

  class Summator extends BaseTypedSystem {
    import sb._
    val numInput  = input [Int]("numInput")
    val getInput  = input [Any]("getInput")
    val sumOutput = output[Int]("sumOutput")

    {
      val sum = state[Int]("sum", 0)

      numInput.addTo(sum)
      getInput.getState(sum) >> sumOutput
      getInput.const(0).delay(1).saveTo(sum)
    }
  }
  class SuperSystem extends BaseTypedSystem {
    import sb._

    val nameInput = input[String]("nameInput")
    val total = output[Int]("total")

    val stringStat = new StringStat
    addSubsystem(stringStat)

    val wordStat = new WordStat
    addSubsystem(wordStat)

    val summator = new Summator
    addSubsystem(summator)

    nameInput.map(name => "Hello, "+name, "hello")  >> stringStat.stringInput
    nameInput >> wordStat.wordInput

    stringStat.wordsOutput >> wordStat.wordInput

    wordStat.lenOutput >> summator.numInput
    summator.sumOutput >> total
  }

  class RxSuper {
    private
    val s = new SuperSystem
    private
    val ss = s.toStaticSystem
    ss.toDotAtLevel(1).saveTo("Example4.dot")
    private
    val rs = new DynamicSystemRx(ss.toDynamicSystem)
    val nameInput = rs.rxInput(s.nameInput)
    val total = rs.rxOutput(s.total)
  }

  println("Example4")
  val rs = new RxSuper
  rs.total.subscribe(s => println("s="+s))
  rs.nameInput.onNext("Vasia")
}
