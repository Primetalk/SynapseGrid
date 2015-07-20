package ru.primetalk.synapse.akka

import ru.primetalk.synapse.core._
import akka.actor._
/**
 * ActorComponent is a component that can be added to any system. When
 * the system is converted to dynamic system this component will be converted to an actor.
 * The state will reside in that actor.
 *
 * Inputs and outputs are the same as in the inner system.
 * @author А.Жижелев
 *
 */
case class ActorComponent(subsystem: StaticSystem,
                               supervisorStrategy: SupervisorStrategy = defaultSupervisorStrategy)
  extends Component
  with ComponentWithInternalStructure {
  def name = subsystem.name

  val inputContacts = subsystem.inputContacts
  val outputContacts = subsystem.outputContacts

  def toStaticSystem: StaticSystem = subsystem
}
