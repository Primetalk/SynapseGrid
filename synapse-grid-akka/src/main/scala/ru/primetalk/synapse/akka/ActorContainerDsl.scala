package ru.primetalk.synapse.akka

import ru.primetalk.synapse.core.syntax._
import ru.primetalk.synapse.core.syntax.given
import akka.actor._

import scala.language.implicitConversions
/** API for a system that can contain actor subsystems.*/
trait ActorContainerDsl {
  /** Prefer to use ActorComponent directly.
    *
    * @return the subsystem itself
    */
  def addActorSubsystem[T](subsystem: T,
                           supervisorStrategy: SupervisorStrategy = defaultSupervisorStrategy)(implicit sb: SystemBuilder, ev: Conversion[T, StaticSystem]): T = {
    sb.addComponent(new ActorComponent(subsystem, supervisorStrategy))
    subsystem
  }
}
