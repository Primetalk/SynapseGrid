///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 *
 * Created: 26.08.13, zhizhelev
 */
package ru.primetalk.synapse

import ru.primetalk.synapse.core.{Contact, StateHandle, SystemBuilder}
import _root_.shapeless._

package object shapeless {

  //  implicit class ShapelessSystemBuilder(sb:SystemBuilder){
  implicit class HNilContact[T](c: Contact[T]) {
    def hNil(implicit sb: SystemBuilder): Contact[T :: HNil] = {
      import sb._
      c.map(value => value :: HNil)
    }
  }

  implicit class ShapelessContact[L <: HList](c: Contact[L]) {
    def using[S](stateHandle: StateHandle[S])(implicit sb: SystemBuilder): Contact[S :: L] = {
      import sb._
      c.withState(stateHandle).stateMap[S :: L](
        (stateValue, message: L) =>
          (stateValue, new ::(stateValue, message))
      )
    }
  }

}
