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

  val b0 = empty[Box].set(width, simple(10))
  val b1 = b0.set(height, simple(20))

  test("b0 doesn't have all properties of boxSchema") {
    assert(!boxSchema.hasAllProperties(b0))
  }

  test("b1 has all properties of boxSchema") {
    assert(boxSchema.hasAllProperties(b1))
  }
  val b2 = b1.set(longId, simple(10L))
  test("b2 has property from parent") {
    assert(simpify(b2.get(longId)) === 10L)
  }
  val boxes = Rel[Global, Seq[Box]]("boxes")
  implicit val boxesSchema = CollectionSchema[Box](boxSchema)
  val globalSchema = record[Global](boxes)

  test("Get meta test") {
    val widthSchema = globalSchema / boxes / Element / width
    assert(widthSchema === SimpleSchema[Int])
  }

  test("b2 matches box schema") {
    assert(isMatching(b2, boxSchema) === Seq())
  }
  test("align raw data") {
    val b3 = align(List(10, 20), boxSchema)
    assert(b3 === b1)
  }
  test("align and unalign") {
    val b1data = unalign(InstanceWithMeta(b1, boxSchema))
    assert(b1data === List(10,20) )
    val b1restored = align(b1data, boxSchema)
    assert(b1restored === b1)
  }

  val globalInstance: relations.RecordInstance[Global] = empty[Global].set(boxes, seq(b0,b1))

  test("navigation through hierarchy") {
    val path0width = boxes / 0 / width
    val path1width = boxes / 1 / width
    val w0 = navigate(globalInstance, path0width)
    val w1 = navigate(globalInstance, path1width)
    assert(w0 === w1)
  }

  val boxBuilder = new Builder(boxSchema)
  boxBuilder.set(width, simple(10))
  boxBuilder.set(height, simple(20))
  val b4 = boxBuilder.toInstance
  test("builder"){
    assert(b1 === b4)
  }

}
