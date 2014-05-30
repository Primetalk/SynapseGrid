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

object resources {

  // IDENTIFICATION

  /** The identifier of a resource of type T. */
  trait ResourceId[+T]

  /** A function that can resolve the resource by it's id. */
  type ResourceResolver[T] = ResourceId[T] => T

  /** The root of absolute path. All other entities are identified
    * relatively of the root. */
  final abstract class Root

  /** The identifier of the root object. */
  object RootId extends ResourceId[Root]

  /** Relative identifier of a child within the parent. */
  case class RelId[T, P](parent: ResourceId[P], child: ResourceId[T]) extends ResourceId[T]

  /** a long identifier of an entity. */
  case class LongId[T](id: Long) extends ResourceId[T]

  // META

  /** Common ancestor for all meta-resources like properties and relations. */
  trait Meta

  /** These meta-meta resources describe Meta resources. */
  trait MetaMeta

  /** A binary relation */
  trait Relation[Left, Right] extends Meta

  /** Relation predicate - it is for forths (четвёрки). Represents some skew to the relation. */
  trait Predicate[Left, Right, R <: Relation[Left, Right]] extends Meta


  /** A property is a relation of one parent object with some
    * other object that can be represented as a literal value. */
  trait Property[Left, Right] extends Meta

  /** Very frequent property - name. */
  case object Name extends Property[Any, String]

  case object Description extends Property[Any, String]

  case object Comment extends Property[Any, String]


  // META-META

  /** An implicit instance of Transitive means that the property is transitive. */
  trait Transitive[T <: Meta] extends MetaMeta

  /** R property is reverse to P. */
  trait Reverse[R <: Meta, P <: Meta] extends MetaMeta

  /** Denotes that the value of property P identifies the entity T */
  trait Identifies[Entity, Property <: Meta] extends MetaMeta


  // DATA representation

  /** Instances of data. */
  trait Triple[Left, Middle, Right]

  /** All components are given with ResourceId's. */
  case class TripleRRR[Left, Middle, Right](left: ResourceId[Left], middle: ResourceId[Middle], right: ResourceId[Right]) extends Triple[Left, Middle, Right]

  /** The value is given with the resource literal. */
  case class TripleRRV[Left, Middle, Right](left: ResourceId[Left], middle: ResourceId[Middle], right: Right) extends Triple[Left, Middle, Right]

  case class PropertyValue[Left, Right](left: ResourceId[Left], middle: Property[Left, Right], right: Right) extends Triple[Left, Property[Left, Right], Right]

}
