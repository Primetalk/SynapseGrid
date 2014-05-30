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

import org.scalatest.FunSuite

class OntologyTest extends FunSuite {

  import resources._

  trait Person

  test("Property value for an instance") {
    val ivan = LongId[Person](1L)
    // name of a person
    PropertyValue(ivan, Name, "Иван")
  }

}
