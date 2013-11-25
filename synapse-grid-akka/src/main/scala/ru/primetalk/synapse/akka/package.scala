package ru.primetalk.synapse

import _root_.akka.actor.{ActorRefFactory,ActorRef}
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
  	/** @param threadSafeOutputFun - a function that will receive output signals of the actor. Should be thread safe!!!*/
    def toActorTree(threadSafeOutputFun:Option[InternalSignals => Any] = None)(implicit actorRefFactory: ActorRefFactory):ActorRef = StaticSystemActor.toActorTree(actorRefFactory)(List(),s, threadSafeOutputFun)
    def toActorTree(implicit actorRefFactory: ActorRefFactory):ActorRef = toActorTree(None)(actorRefFactory)
  }
  
  implicit def toActorSystemBuilder[T<:BasicSystemBuilder](sb:T) = new ActorSystemBuilderOps()(sb) 
  
}
