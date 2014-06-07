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
 * Created: 07.06.14, zhizhelev
 */
package ru.primetalk.synapse.akka.distributed

import akka.actor.{ActorRef, Actor}
import akka.event.{LoggingReceive, Logging}

/** Routes everything except register/unregister commands to
  * the system that has registered with the router.
  */
class RouterBecome extends Actor {
  val log = Logging(context.system, this)

  def receive = LoggingReceive {
    case Register(actor) =>
      log.info("RouterBecome: " + self.path + " received " + Register(actor))
      context.become(LoggingReceive {
        case m =>
          log.info("RouterBecome.recieve[" + self.path + "](" + actor + "): received " + m)
          actor forward m
      })
  }
}

sealed trait RouterCommand

/**
 * Registers the actor as a destination of all Router's input.
 * @param actor the destination of subsequent messages.
 */
case class Register(actor: ActorRef) extends RouterCommand

/**
 * Stops registration. All messages will be sent to DeathWatch.
 */
case object Unregister extends RouterCommand

///** Routes everything except register/unregister commands to
//  * the system that has registered with the router.
//  */
//class Router extends Actor {
//  val log = Logging(context.system, this)
//  var destination: Option[ActorRef] = None
//
//  def receive = {
//    case Register(actor) =>
//      destination = Some(actor)
//    case Unregister =>
//      destination = None
//    case m =>
//      destination.foreach(_ forward m)
//  }
//}
//
