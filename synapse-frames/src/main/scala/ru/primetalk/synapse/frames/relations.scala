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
import scala.reflect._
import scala.collection.mutable

/**
 * Relations are named binary relations between two types.
 *
 * The types are either artificial classes that form entity hierarchy,
 * or ordinary types that usually serve as values.
 *
 * Two generic arguments allow to do complete type-checked property assignements. Having
 * left type we know which entity has the property. Having right type we know what kind of
 * property it is. Thus we can represent type checked hierarchical data structures
 * that resemble JSON.
 *
 * The actual set of properties are described with Schema's. Every schema describes the type T.
 * - if the type is an ordinary type, then schema is SimpleSchema and contains only runtime type information (RTTI).
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

  /**
   * An arbitrary relation identifier.
   **/
  sealed trait Relation[-L, R]

  /** A single instance of type R can be traversed to
    * from  an instance of type L using the given name.
    *
    * Usually this is referred to as an attribute or a property.
    * */
  case class Rel[-L, R](name: String) extends Relation[L, R]

  /** A single instance of type R can be obtained
    * from an  instance of type L using index.
    *
    * Usually this is referred to as a relative identifier of a child instance.
    * */
  case class LongId[T](id: Long) extends Relation[Seq[T], T]

  case class IntId[T](id: Int) extends Relation[Seq[T], T]

  /** Sometimes we can index elements with their key property and
    * select using given property value. */
  case class PropValueId[T, TId](keyProperty: Relation[T, TId], value: TId) extends Relation[Seq[T], T]

  /**
   * A special relation between the collection and its's element.
   * Can be used when we do not know exact identifier of an object within the collection.
   * This can be the case when we add elements to a collection, or we are interested
   * in the schema of elements.
   **/
  case class ElemRel[T]() extends Relation[Seq[T], T]

  /** A composition of two relations. */
  case class Rel2[-L, M, R](_1: Relation[L, M], _2: Relation[M, R]) extends Relation[L, R]

}

trait RelationOps extends RelationsDefs {

  implicit class RelEx[-L, R](r: Relation[L, R]) {
    def /[R2](r2: Relation[R, R2]): Relation[L, R2] = Rel2[L, R, R2](r, r2)
  }

  //  implicit class CollectionEx[T](r: Relation[Seq[T], T]) {
  //    def #[R2](r2: Relation[R, R2]): Relation[L, R2] = Rel2[L, R, R2](r, r2)
  //  }

}

trait SchemaDefs extends RelationsDefs with RelationOps {

  /** an aggregate of a relation with the schema for the right part. */
  case class RelWithRSchema[-L, R](rel: Rel[L, R], schema: Schema[R])

  sealed trait Schema[T] {
    def classTag: ClassTag[T]
  }

  case class SimpleSchema[T](implicit val classTag: ClassTag[T]) extends Schema[T]

  /** Schema that describes some record with properties. The properties can only belong to
    * the type T. However, there can be different set of properties.
    * */
  case class RecordSchema[T](props: Seq[RelWithRSchema[T, _]])(implicit val classTag: ClassTag[T]) extends Schema[T] {
    lazy val map = props.map(p => (p.rel.name, p.schema.asInstanceOf[Schema[_]])).toMap[String, Schema[_]]
  }

  sealed class Tag[Tg, T]

  /** the instance is also tagged with the tag. */
  case class TaggedSchema[Tg, T](tag: Tg, schema: Schema[T]) extends Schema[Tag[Tg, T]] {
    def classTag = scala.reflect.classTag[Tag[Tg, T]]
  }

  case class CollectionSchema[T](elementSchema: Schema[T]) extends Schema[Seq[T]] {
    def classTag = scala.reflect.classTag[Seq[T]]
  }


  case class Tuple2Schema[T1, T2](_1: Schema[T1], _2: Schema[T2]) extends Schema[(T1, T2)] {
    def classTag = scala.reflect.classTag[(T1, T2)]
  }


  case class AnnotatedSchema[T](schema: Schema[T])(annotations: Any*) extends Schema[T] {
    def classTag = schema.classTag
  }

  //  case class PlainSchema(schemas: List[(TypeTag[T], Schema[T]) forSome {type T}])

  case class Gen1Schema[T, S[T]](tag: Any, schema: Schema[T])(implicit val classTag: ClassTag[S[T]]) extends Schema[S[T]]

  /** The user may use another way of type description. */
  case class CustomSchema[T, CS](tag: Any, cs: CS)(implicit val classTag: ClassTag[T]) extends Schema[T]


}

trait InstanceDefs extends SchemaDefs {

  /** An instance that is associated with the type T. */
  sealed trait Instance[T] {
    type SchemaType <: Schema[T]
  }

  case class SimpleInstance[T](value: T) extends Instance[T] {
    type SchemaType = SimpleSchema[T]
  }

  case class RecordInstance[T](map: Map[String, Instance[_]]) extends Instance[T] {
    type SchemaType = RecordSchema[T]
    //    lazy val map = values.toMap
    lazy val keySet = map.keySet
    lazy val values = map.toSeq

    def get[V](rel: Rel[T, V]): Instance[V] =
      map(rel.name).asInstanceOf[Instance[V]]

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

  case class InstanceWithMeta[T](i: Instance[T], s: Schema[T])

}

trait SimpleOperationsDefs extends InstanceDefs {
  // this implicit leads to v
  //  implicit def toInstance[T](value: T)(implicit simpleSchema: SimpleSchema[T]) = SimpleInstance[T](value)

  implicit def toExistentialRelWithRSchema[T, T2](rel: Rel[T, T2])(implicit schema: Schema[T2]): RelWithRSchema[T, _] = RelWithRSchema(rel, schema)

  implicit def toSimpleSchema[T <: AnyVal](implicit classTag: ClassTag[T]) = SimpleSchema[T]()

  implicit def intToIntId[T](id: Int) = IntId[T](id)

  def navigate[E, T](i: Instance[E], path: Relation[E, T]): Instance[T] =
    (
      (i, path) match {
        case (r: RecordInstance[_], p: Rel[_, _]) => r.get(p)
        case (c: CollectionInstance[_], IntId(id)) => c.values(id)
        case (_, Rel2(_1, _2)) => navigate(navigate(i, _1), _2)
        case _ => throw new IllegalArgumentException(s"Couldn't navigate from $i by $path")
      }
      ).asInstanceOf[Instance[T]]

  implicit class RecordInstanceOps[T](i: RecordInstance[T]) {
    def set[T2, Anc >: T](prop: Rel[Anc, T2], value: Instance[T2]): RecordInstance[T] =
      i.copy(i.map + ((prop.name, value)))

    //    def set[T2, Anc >: T](prop: Rel[Anc, T2], value: T2): RecordInstance[T] =
    //      i.copy(i.values :+(prop.name, SimpleInstance(value)))

    //    def get[T2, Anc >: T](prop: Rel[Anc, T2]) = i.map(prop.name)
    //    def get[T2, Anc >: T](path: Relation[Anc, T2]) = path match {
    //      case r@Rel => get(r)
    //      case Rel2(_1, _2) =>
    //        get(_1)
    //    }
  }

  def simple[T](value: T) = SimpleInstance(value)

  def simpify[T](i: Instance[T]) = i match {
    case SimpleInstance(value) =>
      value.asInstanceOf[T]
    case _ => throw new IllegalArgumentException(s"$i cannot be simplified")
  }

  def empty[T](implicit classTag: ClassTag[T]): RecordInstance[T] = RecordInstance[T](Map())

  def record[T](props: RelWithRSchema[T, _]*)(implicit classTag: ClassTag[T]) = RecordSchema[T](props.toSeq)

  def seq[T](values: Instance[T]*): CollectionInstance[T] = CollectionInstance[T](values.toSeq)

  class Builder[T](schema: RecordSchema[T]) {
    private
    val map = mutable.Map[String, Instance[_]]()

    def set[T2, Anc >: T](prop: Rel[Anc, T2], value: Instance[T2]) = {
      if (schema.map.keySet.contains(prop.name))
        map(prop.name) = value
      else
        throw new IllegalArgumentException(s"Schema $schema doesn't contain ${prop.name}")
      this
    }

    def toInstance = {
      val diff = schema.map.keySet -- map.keySet
      if (diff.isEmpty)
        RecordInstance[T](map.toMap)
      else
        throw new IllegalArgumentException(s"The builder doesn't yet contain the following properties: $diff")
    }
  }

}

/** operations with schema instances. */
trait OperationsDefs extends SimpleOperationsDefs {

  implicit class RecordSchemaEx[T](schema: RecordSchema[T]) {
    def hasAllProperties(i: RecordInstance[T]) =
      schema.props.map(_.rel.name).forall(i.keySet.contains)
  }

  /** Checks match and returns Seq() if matches. Otherwise returns the list of non-matching
    * elements. */
  def isMatching[T](i: Instance[T], schema: Schema[T]): Seq[InstanceWithMeta[_]] = {
    def isMatching0(p: InstanceWithMeta[_]): Seq[InstanceWithMeta[_]] = p match {
      case InstanceWithMeta(i: SimpleInstance[_], s: SimpleSchema[_]) =>

        if (s.classTag.runtimeClass.isPrimitive //TODO: implement for primitive types
          || s.classTag.runtimeClass.isAssignableFrom(i.value.getClass))
          Seq()
        else
          Seq(p)
      case InstanceWithMeta(r@RecordInstance(_), s@RecordSchema(props)) =>
        for {
          v <- r.values
          if s.map.contains(v._1)
          iwm = InstanceWithMeta(v._2.asInstanceOf[Instance[Any]], s.map(v._1).asInstanceOf[Schema[Any]])
          matchResult <- isMatching0(iwm)
        } yield matchResult
      case InstanceWithMeta(i, AnnotatedSchema(s)) =>
        isMatching0(InstanceWithMeta(i, s))
      case _ =>
        throw new IllegalArgumentException("isMatching is not implemented for " + p)
    }
    isMatching0(InstanceWithMeta(i, schema))
  }


  /** Aligns raw tuple (list of any) with the schema. Every data element
    * is attached to apropriate property of the schema. */
  def align[T](data: List[Any], schema: Schema[T]): Instance[T] = {
    def align0(data: List[Any], schema: Schema[_]): (Instance[T], List[Any]) = schema match {
      case s: SimpleSchema[_] =>
        (SimpleInstance(data.head.asInstanceOf[T]), data.tail)
      case RecordSchema(propSeq) =>
        def align1(data: List[Any],
                   props: List[RelWithRSchema[_, _]],
                   res: List[(String, Instance[T])]): (List[(String, Instance[_])], List[Any]) =
          props match {
            case Nil => (res.reverse, data)
            case RelWithRSchema(rel, schema) :: ptail =>
              val (prop, rest) = align0(data, schema)
              align1(rest, ptail, (rel.name, prop) :: res)
            case msg :: _ => throw new IllegalArgumentException(s"Alignment is not implemented for $msg")
          }
        val (props, tail) = align1(data, propSeq.toList, Nil)
        (RecordInstance(props.toMap), tail)
      case AnnotatedSchema(s) =>
        align0(data, s)
      case _ => throw new IllegalArgumentException(s"Alignment is not implemented for $schema")
    }
    val (res, tail) = align0(data, schema)
    if (!tail.isEmpty)
      throw new IllegalArgumentException(s"$data cannot be aligned to $schema completely")
    res
  }

  /** Converts schema-based instance to a raw tuple (list of any).
    */
  def unalign[T](data: InstanceWithMeta[T]): List[Any] = data match {
    case InstanceWithMeta(SimpleInstance(v), _) =>
      List(v)
    case InstanceWithMeta(i@RecordInstance(values), RecordSchema(propSeq)) =>
      propSeq.toList.flatMap { case RelWithRSchema(rel, s) =>
        val value = i.get(rel)
        unalign(InstanceWithMeta(value, s))
      }
    case InstanceWithMeta(i, AnnotatedSchema(s)) =>
      unalign(InstanceWithMeta(i, s))
    case _ =>
      throw new IllegalArgumentException(s"Unalignment is not implemented for $data")
  }

  def flatten[T](data: InstanceWithMeta[T]): List[Any] = unalign(data)
}

trait Navigation extends InstanceDefs {

  implicit class SchemaEx[T](schema: Schema[T]) {
    def /[T2, Anc >: T](prop: Rel[Anc, T2]): Schema[T2] = schema match {
      case r: RecordSchema[T] => r.map(prop.name).asInstanceOf[Schema[T2]]
      case _ => throw new IllegalArgumentException(s"cannot proceed hierachically with other schemas except RecordSchema or CollectionSchema: $schema ")
    }

  }

  object Element

  implicit class SchemaEx2[T](schema: Schema[Seq[T]]) {

    def /(e: Element.type): Schema[T] = schema match {
      case cs: CollectionSchema[T] => cs.elementSchema.asInstanceOf[Schema[T]]
      case _ => throw new IllegalArgumentException(s"Cannot proceed hierachically with other schemas except CollectionSchema: $schema ")
    }

  }

}

object relations
  extends RelationsDefs
  with SchemaDefs
  with InstanceDefs
  with Navigation
  with OperationsDefs {

}
