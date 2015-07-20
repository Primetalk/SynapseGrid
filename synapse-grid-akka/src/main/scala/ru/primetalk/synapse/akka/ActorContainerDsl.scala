package ru.primetalk.synapse.akka

import ru.primetalk.synapse.core._
import akka.actor._
/** API for a system that can contain actor subsystems.*/
trait ActorContainerDsl {
  /** Prefer to use ActorComponent directly.
    *
    * @return the subsystem itself
    */
  def addActorSubsystem[T](subsystem: T,
                           supervisorStrategy: SupervisorStrategy = defaultSupervisorStrategy)(implicit sb: SystemBuilder, ev: T => StaticSystem): T = {
    sb.addComponent(new ActorComponent(subsystem, supervisorStrategy))
    subsystem
  }
}
