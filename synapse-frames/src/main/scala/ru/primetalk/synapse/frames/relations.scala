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
 * Created: 02.07.14, zhizhelev
 */
package ru.primetalk.synapse.frames

import scala.reflect.runtime.universe._

object relations {

  case class Rel[L, R](name: String)

  sealed trait Schema[T]

  case class SimpleSchema[T](implicit typeTag: TypeTag[T]) extends Schema[T]

  case class CollectionSchema[T](elementSchema: Schema[T]) extends Schema[Seq[T]]

  /** an aggregate of a relation with the schema for the right part. */
  case class RelWithRSchema[L, R](rel: Rel[L, R], schema: Schema[R])

  case class CompositeSchema[T](props: Seq[RelWithRSchema[T, _]]) extends Schema[T]

  case class PlainSchema(schemas: List[(TypeTag[T], Schema[T]) forSome {type T}])

}
