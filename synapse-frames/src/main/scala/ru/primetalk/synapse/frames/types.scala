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
 * Created: 11.05.14, zhizhelev
 */
package ru.primetalk.synapse.frames

object types {

  /** Some operations with types that can be helpful when dealing with types.
    *
    * @see http://www.chuusai.com/2011/06/09/scala-union-types-curry-howard/
    *      Unboxed union types in Scala via the Curry-Howard isomorphism by Miles Sabin. */
  object TypeOps {

    type ¬[A] = A => Nothing
    type ¬¬[A] = ¬[¬[A]]

    type ∈[T, P] = T <:< P
    type ∉[T, P] = ¬[P] <:< ¬[T]

  }

  /** analog of shapeless HList */
  sealed trait HList {
    self =>
    def ::[T](head: T) = types.::[T, self.type](head, self)
  }

  case object HNil extends HList

  type HNil = HNil.type

  case class ::[T, L <: HList](head: T, tail: L) extends HList

  implicit class HListOps[L <: HList](hlist: L) {
    def toList: List[Any] = {
      def toList0(hlist0: HList): List[Any] = {
        hlist0 match {
          case HNil => Nil
          case ::(h, t) => h :: toList0(t)
        }
      }
      toList0(hlist)
    }
  }


}
