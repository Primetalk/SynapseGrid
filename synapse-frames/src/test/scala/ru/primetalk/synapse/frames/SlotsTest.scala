///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2014                                   //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2014.
 * All rights reserved.
 * Authors: A.Zhizhelev
 *
 * Created: 25.04.14, zhizhelev
 */
package ru.primetalk.synapse.frames

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.language.existentials

@RunWith(classOf[JUnitRunner])
class SlotsTest extends FunSuite {

  import ru.primetalk.synapse.frames.SlotsOntology._
  import ru.primetalk.synapse.frames.slots._

  test("slots test") {
    val ps = name :: SNil
    val pv = name := "Vasya"
    val ei = pv :: EmptySlotSeqValueBuilder

    trait Named {
      val name = slot[String]("name")
    }

    object Person extends Named {
      val maritalStatus = slot[Boolean]("maritalStatus")
      val maritalPartner = slot[String]("maritalPartner")
    }

    object Registrar extends Named

    val marry =
      (Person.name := "Maria") ::
        (Person.name := "Vasya") ::
        (Registrar.name := "ЗАГС Петроградского района") ::
        EmptySlotSeqValueBuilder

    //    val i1 = pv :: ei
    //    val name = Prope
    //    val i1 =
    def handleCompileTime(marryInfo: marry.type) {
      val wife = marryInfo.head.value
      val husband = marryInfo.tail.head.value
      val registrar = marryInfo.tail.tail.head.value

      val maritalStatusW =
        (Person.name := wife) ::
          (Person.maritalStatus := true) ::
          (Person.maritalPartner := husband) ::
          EmptySlotSeqValueBuilder

      val maritalStatusH =
        (Person.name := husband) ::
          (Person.maritalStatus := true) ::
          (Person.maritalPartner := wife) ::
          EmptySlotSeqValueBuilder

      //      PersonDb.save(maritalStatusW)
      //      PersonDb.save(maritalStatusH)
    }

  }

}
