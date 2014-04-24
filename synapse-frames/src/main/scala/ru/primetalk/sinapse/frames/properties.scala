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
 * Created: 21.04.14, zhizhelev
 */
package ru.primetalk.sinapse.frames

object properties {

  /** Domain - is a kind of restrictions on values of type T.
    *
    * For instance: varchar(10) - is a domain for String with constraint length<=10
    * int - is a domain for number with a predefined range.
    *
    * Different domains have different incompatible types.
    * There can only be a conversion between domain value
    * and
    * We do not create other types except Domain[T].
    * All constructors should return only this super type (by
    * adding a type constraint :Domain[T] where necessary.)
    */
  trait Domain0 {
    type Type
  }

  trait Domain[T] extends Domain0 {
    type Type = T
  }

  /** Identifier of a property with values of a domain.
    *
    * (We cannot associate a property with a Type directly, becase
    * we need the domain.)
    *
    * All properties of the same domain have the same ancestor type.
    * They are compatible to some extent.
    * However, they can be differentiated by corresponding instance type.
    *
    * When an instance of a propertyId is created, it also creates unique type (this.type).
    *
    * Using this type we can construct a union of properties - PropertySet, and then check
    * at compile time whether a property can be used in instances of the corresponding PropertySet.
    * */
  trait PropertyId {
    type Type
    type D <: Domain[Type]

    /** The domain of the property. */
    def domain: D

    def :=(value: Type): PropertyValue0 = PropertyValue[Type, D, this.type](this, value)
  }

  type PropertyIdForDomain[T, D1 <: Domain[T]] = PropertyId {type Type = T; type D = D1}

  sealed trait PropertyValue0 {
    type D <: Domain[Type]
    //{type Type = this.type}
    type Type
    type P <: PropertyIdForDomain[Type, D]
  }

  /** The value of a property. */

  case class PropertyValue[T, D1 <: Domain[T], P1 <: PropertyIdForDomain[T, D1]]
  (propertyId: P1, value: T) extends PropertyValue0 {
    type Type = T
    type D = D1
    type P = P1
  }

  /** The value of a domain. */
  case class DomainValue[T, D <: Domain[T]](domain: D, value: T)


  //  /** Entity type is determined by domains of it's properties.
  //    * A kind of HList can be used to represent entity type.
  //    *
  //    * Here, however, we only want to define entity object properties.
  //    */
  //  trait Entity {
  //    def properties:Seq[PropertyId]
  //  }
  //
  sealed trait PropertySet {
    type PropertyUnion

    def props: Seq[PropertyId]
  }

  case object EmptyPropertySet extends PropertySet {
    type PropertyUnion = Any

    def props: Seq[PropertyId] = Seq()
  }

  case class PairPropertySet[P <: PropertyId, PS <: PropertySet](propId: P, tail: PS) extends PropertySet {
    type PropertyUnion = PS#PropertyUnion with P

    def props: Seq[PropertyId] = propId +: tail.props
  }

  /** An instance can contain PropertyValues for any subset of properties defined in a PropertySet. */
  sealed trait Instance {
    type PS <: PropertySet

    def propertySet: PS
  }

  /** An empty instance for the given property set. */
  case class EmptyInstance[PS1 <: PropertySet](propertySet: PS1) extends Instance {
    type PS = PS1

  }

  case class PairInstance[P1 <: PropertyId, I <: Instance](propValue: PropertyValue0 {type P <: P1}, rest: I) {
    type PS = I#PS

    def propertySet = rest.propertySet
  }

}
