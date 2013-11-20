package ru.primetalk.synapse

import _root_.akka.actor.ActorRefFactory
import ru.primetalk.synapse.core._
import scala.language.implicitConversions

///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 * (2-clause BSD license) See LICENSE
 *
 * Created: 01.07.13, zhizhelev
 */
package object akka {
  implicit class RichStaticSystemSystem(s:StaticSystem){
    def toActorTree(implicit actorRefFactory: ActorRefFactory) = StaticSystemActor.toActorTree(actorRefFactory)(List(),s)
  }
  
  implicit def toActorSystemBuilder[T<:BasicSystemBuilder](sb:T) = new ActorSystemBuilderOps()(sb) 
  
}
