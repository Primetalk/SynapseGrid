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

import scala.language.{existentials, higherKinds, implicitConversions, reflectiveCalls}
import scala.reflect.runtime.universe._

/**
 * Relations are named binary relations between two types.
 * The types are either artificial classes that form entity hierarchy,
 * or ordinary types that usually serve as values.
 *
 * Two generic arguments allows to do complete type-checked property assignements. Having
 * left type we know which entity has the property. Having right type we know what kind of
 * property it is. Thus we can represent type checked hierarchical structures that resemble JSON.
 *
 * The actual set of properties are described with Schema's. Every schema describes the type T.
 * - if the type is an ordinary type, then schema is SimpleSchema and contains runtime type information (RTTI).
 *
 * - if we want to associate some structured data with the type, we use RecordSchema.
 *
 * - to combine two schemas (compose/aggregate) we use Tuple2Schema which constructs schema that describes
 * a pair of types with the schemas for either types.
 *
 * The values of types according to schema can be represented in different forms. However, general data processing
 * can be done with the set of Instance[T] desendents.
 *
 * It is also possible to have reflection-based implementation for storing data in fields of POJO.
 */
trait RelationsDefs {

  case class Rel[+L, R](name: String)

}

trait SchemaDefs extends RelationsDefs {

  /** an aggregate of a relation with the schema for the right part. */
  case class RelWithRSchema[L, R](rel: Rel[L, R], schema: Schema[R])

  sealed trait Schema[T]

  case class SimpleSchema[T](implicit typeTag: TypeTag[T]) extends Schema[T]

  case class RecordSchema[T](props: Seq[RelWithRSchema[T, _]]) extends Schema[T] {
    lazy val map = props.map(p => (p.rel.name, p.schema.asInstanceOf[Schema[_]])).toMap[String, Schema[_]]
  }

  case class CollectionSchema[T](elementSchema: Schema[T]) extends Schema[Seq[T]]


  case class Tuple2Schema[T1, T2](_1: Schema[T1], _2: Schema[T2]) extends Schema[(T1, T2)]

  final class Tag[Tg, T]

  case class TaggedSchema[Tg, T](tag: Tg, schema: Schema[T]) extends Schema[Tag[Tg, T]]

  case class AnnotatedSchema[T](schema: Schema[T])(annotations: Any*) extends Schema[T]

  //  case class PlainSchema(schemas: List[(TypeTag[T], Schema[T]) forSome {type T}])

  case class Gen1Schema[T, S[T]](tag: Any, schema: Schema[T]) extends Schema[S[T]]

  /** The user may use another way of type description. */
  case class CustomSchema[T, CS](tag: Any, cs: CS) extends Schema[T]


}

trait InstanceDefs extends SchemaDefs {

  /** An instance that is associated with the type T. */
  sealed trait Instance[T] {
    type SchemaType <: Schema[T]
  }

  case class SimpleInstance[T](value: T) extends Instance[T] {
    type SchemaType = SimpleSchema[T]
  }

  case class RecordInstance[T](values: Seq[(String, Instance[_])]) extends Instance[T] {
    type SchemaType = RecordSchema[T]
    lazy val map = values.toMap
    lazy val keySet = map.keySet
  }

  case class CollectionInstance[T](values: Seq[Instance[T]]) extends Instance[Seq[T]] {
    type SchemaType = CollectionSchema[T]
  }

  case class Tuple2Instance[T1, T2](value: (Instance[T1], Instance[T2])) extends Instance[(T1, T2)] {
    type SchemaType = Tuple2Schema[T1, T2]
  }

  case class TaggedInstance[Tg, T](tag: Tg, value: Instance[T]) extends Instance[Tag[Tg, T]] {
    type SchemaType = TaggedSchema[Tg, T]
  }

  case class AnnotatedInstance[T](value: Instance[T]) extends Instance[T] {
    type SchemaType = AnnotatedSchema[T]
  }

  case class Gen1Instance[T, S[T]](tag: Any, value: Instance[T]) extends Instance[S[T]] {
    type SchemaType = Gen1Schema[T, S]
  }

  case class CustomInstance[T, CS](tag: Any, value: Any) extends Instance[T] {
    type SchemaType = CustomSchema[T, CS]
  }

}

/** operations with schema instances. */
trait OperationsDefs extends InstanceDefs {
  implicit def toInstance[T](value: T)(implicit simpleSchema: SimpleSchema[T]) = SimpleInstance[T](value)

  implicit def toExistentialRelWithRSchema[T, T2](rel: Rel[T, T2])(implicit schema: Schema[T2]): RelWithRSchema[T, _] = RelWithRSchema(rel, schema)

  implicit def toSimpleSchema[T <: AnyVal](implicit typeTag: TypeTag[T]) = SimpleSchema[T]()

  implicit class RecordInstanceOps[T](i: RecordInstance[T]) {
    def set[T2, Anc >: T](prop: Rel[Anc, T2], value: Instance[T2]): RecordInstance[T] =
      i.copy(i.values :+(prop.name, value))

    def get[T2, Anc >: T](prop: Rel[Anc, T2]) = i.map(prop.name)
  }

  def simpify[T](i: Instance[T]) = i match {
    case SimpleInstance(value) =>
      value.asInstanceOf[T]
    case _ => throw new IllegalArgumentException(s"$i cannot be simplified")
  }

  def empty[T]: RecordInstance[T] = RecordInstance[T](Seq())

  def record[T](props: RelWithRSchema[T, _]*) = RecordSchema[T](props.toSeq)

  implicit class RecordSchemaEx[T](schema: RecordSchema[T]) {
    def hasAllProperties(i: RecordInstance[T]) =
      schema.props.map(_.rel.name).forall(i.keySet.contains)
  }

  def isMatching[T](i: Instance[T], schema: Schema[T]): Boolean = {
    def isMatching0(p: (Instance[T], Schema[T])): Boolean = {
      ???
    }
    isMatching0((i, schema))
  }

  implicit class SchemaEx[T](schema: Schema[T]) {
    def /[T2, Anc >: T](prop: Rel[Anc, T2]): Schema[T2] = schema match {
      case r: RecordSchema[T] => r.map(prop.name).asInstanceOf[Schema[T2]]
      case _ => throw new IllegalArgumentException(s"cannot proceed hierachically with other schemas except RecordSchema: $schema ")
    }

    def element = schema match {
      case cs: CollectionSchema[T] => cs.elementSchema.asInstanceOf[Schema[T]]
      case _ => throw new IllegalArgumentException(s"cannot proceed hierachically with other schemas except RecordSchema: $schema ")
    }
  }

}

object relations
  extends RelationsDefs
  with SchemaDefs
  with InstanceDefs
  with OperationsDefs {

}
