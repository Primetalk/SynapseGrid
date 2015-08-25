///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * ${PROJECT_NAME}
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 * (2-clause BSD license) See LICENSE
 *
 * Created: 01.07.13, zhizhelev
 */
package ru.primetalk.synapse.akka

import akka.actor.{ActorContext, ActorRef}
import ru.primetalk.synapse.core.Contact

object SpecialActorContacts {

  /** A special contact that will get sender if inputs contain it. */
  object SenderInput extends Contact[ActorRef]

  /** This contact receives Actor's context at the very beginning. */
  object ContextInput extends Contact[ActorContext]

  /** Some contact that can receive some info before the actor is started. */
  object PreStartInput extends Contact[Any]

  /**
   * A special contact that will get external messages that are not represented with signals.
   * It also receives the sender of the message.
   * it is commonly used as a way to handle external messages from other actors that
   * know nothing about SynapseGrid.
   */
  object NonSignalWithSenderInput extends Contact[(ActorRef, Any)]

  /** The system that wants current time can put this contact in inputs and will obtain ticks with current time. */
  object CurrentTimeMsInput extends Contact[Long]

  sealed trait LifeControlCommand

  /** Should prepare a system for termination.
    * After processing this signal all further messages won't go into the system.
    * They should be ignored if some of them do.
    *
    * Anyway, the system should release resources and it should dispatch control signal further to children.
    * And the system itself should produce neither signals anymore.
    *
    * When getting this signal the system should answer with LifeControlCommandReport.
    * */
  case object PreDie extends LifeControlCommand

  case object Die extends LifeControlCommand

  case object Reset extends LifeControlCommand

  case object PostStop extends LifeControlCommand

  /** */
  case class LifeControlCommandReport(cmd: LifeControlCommand, failureDescription: Option[String])

  sealed trait InitializationReport

  /** Special actor message to send to parent when the actor has been initialized. */
  case class InitCompleted(actor: ActorRef) extends InitializationReport

  /**
   * If actor couldn't initialize itself due to some exception.
   * There can also be timeout for initialization. In this case
   * The actor can receive `stop` after InitSuccess
   */
  case class InitFailed(actor: ActorRef, cause: Throwable) extends InitializationReport

  sealed trait LifeCycleReport

  case object LifeControl extends Contact[LifeControlCommand]

  case object PreDieControl extends Contact[LifeControlCommand]

  case object LifeControlReport extends Contact[LifeControlCommandReport]

  case object PostStopInput extends Contact[LifeControlCommand]


}
