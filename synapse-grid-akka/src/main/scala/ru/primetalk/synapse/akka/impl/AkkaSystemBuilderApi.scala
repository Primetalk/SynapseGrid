package ru.primetalk.synapse.akka.impl

import akka.actor.{AllForOneStrategy, Actor, ActorRef, SupervisorStrategy}
import ru.primetalk.synapse.core.{SystemBuilderC, Contact, StaticSystem, BasicSystemBuilder}
import scala.language.implicitConversions

/**
 * @author zhizhelev, 25.03.15.
 */
trait AkkaSystemBuilderApi {
  val defaultSupervisorStrategy: SupervisorStrategy = AllForOneStrategy() {
    case _: Throwable => SupervisorStrategy.Escalate
  }

  class ActorSystemBuilderOps(implicit sb: BasicSystemBuilder) {
    @deprecated("use system.toActorComponent", "26.03.2015")
    def addActorSubsystem[T](subsystem: T,
                             supervisorStrategy: SupervisorStrategy =
                             defaultSupervisorStrategy
                              )(implicit
                                ev: T => StaticSystem): T = {
      sb.addComponent(new ru.primetalk.synapse.akka.ActorComponent(subsystem, supervisorStrategy))
      subsystem
    }

    /**
     * Create subsystem for the child actor.
     *
     * @param factory create actor using the supplied parent reference.
     */
    def childActorAdapterSnippet[TInput, TOutput](name: String,
                                                  input: Contact[TInput], outputContact: Contact[TOutput])(factory: ActorRef ⇒ Actor): StaticSystem = {
      sb.inputs(ru.primetalk.synapse.akka.SpecialActorContacts.NonSignalWithSenderInput)
      val innerSb = new SystemBuilderC(name)
      new ru.primetalk.synapse.akka.ChildActorAdapterSnippet(name, input, outputContact)(factory)(innerSb)
      innerSb.toStaticSystem
    }

  }
  implicit def toActorSystemBuilder[T <: BasicSystemBuilder](sb: T):ActorSystemBuilderOps = new ActorSystemBuilderOps()(sb)


}
