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
 * Created: 30.05.14, zhizhelev
 */
package ru.primetalk.synapse.ontology

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class OntologyTest extends FunSuite {

  import ru.primetalk.synapse.ontology.resources._

  trait Person

  test("Property value for an instance") {
    val ivan = LongId[Person](1L)
    // name of a person
    PropertyValue(ivan, Name, "Иван")
  }

}
