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
 * Created: 26.06.14, zhizhelev
 */
package ru.primetalk.synapse.frames

import org.scalatest.FunSuite

class RelationsTest extends FunSuite {

  import ru.primetalk.synapse.frames.relations._

  //  implicit val int  = SimpleSchema[Int]
  //  implicit val long = SimpleSchema[Long]

  trait Identified

  class Box extends Identified

  class Global

  val longId = Rel[Identified, Long]("id")
  val width = Rel[Box, Int]("width")
  val height = Rel[Box, Int]("height")

  val boxSchema = record[Box](width, height)

  val b0 = empty[Box].set(width, 10)
  val b1 = b0.set(height, 20)

  test("b0 doesn't have all properties of boxSchema") {
    assert(!boxSchema.hasAllProperties(b0))
  }

  test("b1 has all properties of boxSchema") {
    assert(boxSchema.hasAllProperties(b1))
  }
  val b2 = b1.set(longId, 10L)
  test("b2 has property from parent") {
    assert(simpify(b2.get(longId)) === 10L)
  }
  val boxes = Rel[Global, Seq[Box]]("boxes")
  implicit val boxesSchema = CollectionSchema[Box](boxSchema)
  val globalSchema = record[Global](boxes)

  test("Get meta test") {
    val widthSchema = (globalSchema / boxes).element / width
    assert(widthSchema === SimpleSchema[Int])
  }

}
