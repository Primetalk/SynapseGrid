///////////////////////////////////////////////////////////////
// Речевой портал                                            //
// © ООО «Праймтолк», 2011-2013                              //
// Авторы: Жижелев А.А., Нехаев А.Р., Попов П.А.             //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * Speech portal
 * © Primetalk Ltd., 2011-2013.
 * Authors: Zhizhelev A., Nehaev A., Popov P.
 * All rights reserved.
 * Created: 31.03.2013
 */
package ru.primetalk.synapse.akka

import ru.primetalk.synapse.core._
import akka.actor._
import ru.primetalk.synapse.akka.SpecialActorContacts.{NonSignalWithSenderInput, ContextInput, SenderInput}
import ru.primetalk.synapse.slf4j.SystemBuilderWithLogging

/**
 * StaticSystemActorAdapter is a component that can be added to any system. When
 * the system is converted to dynamic system this component will be converted to an actor
 * an actor will be constructed. The state will reside in that actor.
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

trait ActorContainerBuilder extends SystemBuilder {
  /** Prefer to use StaticSystemActorAdapter directly.
    * 
    * @return the subsystem itself
    */
  def addActorSubsystem[T](subsystem: T,
                           supervisorStrategy: SupervisorStrategy = defaultSupervisorStrategy)(implicit ev: T => StaticSystem): T = {
    addComponent(new ActorComponent(subsystem, supervisorStrategy))
    subsystem
  }

  /**
   * Create subsystem for the child actor.
   *
   * @param factory create actor using the supplied parent reference.
   */
  def childActorAdapterSnippet[TInput, TOutput](name: String,
                                                input: Contact[TInput], outputContact: Contact[TOutput])(factory: ActorRef ⇒ Actor): StaticSystem = {
    inputs(NonSignalWithSenderInput)
    new ChildActorAdapterSnippet(name, input, outputContact)(factory).toStaticSystem
  }
}

/** Basic builder that defines a few helpers for constructing actor-held systems. */
trait ActorSystemBuilder extends ActorContainerBuilder {
  lazy val sender = {
    val sender1 = state[ActorRef]("sender", akka.actor.Actor.noSender)

    inputs(SenderInput)
    SenderInput.saveTo(sender1)
    sender1
  }

  lazy val context = {
    val context1 = state[ActorContext]("context", null)
    inputs(ContextInput)
    ContextInput.saveTo(context1)
    context1
  }

  lazy val self = {
    val self1 = state[ActorRef]("self", akka.actor.Actor.noSender)

    inputs(ContextInput)
    ContextInput map(_.self, "_.self") saveTo self1
    self1
  }
  lazy val SelfInput = {
    val SelfInput1 = contact[ActorRef]("SelfInput")

    inputs(ContextInput)
    ContextInput -> SelfInput1 map(_.self, "_.self")
    SelfInput1
  }


  implicit class ImplRichContactUnzipperToActor[T](c: Contact[(ActorRef, T)]) {
    //extends ImplRichContact[(ActorRef, T)](c) {
    def tellToActor(actor: ActorRef, name: String = "") = {
      c foreach(p => actor.tell(p._2, p._1), nextLabel(name, "tellToActor(" + actor.path + ")"))
    }

    def tellToActorFromSelf(actor: ActorRef, name: String = "") = {
      c from self tellToActor(actor, name)
    }
  }

  class ImplRichContactActor[T](c: Contact[T]) {
    def toActorIndirect(actorRefState: StateHandle[ActorRef], name: String = "") = {
      c.from(self).
        labelNext("to @" + actorRefState).
        zipWithState(actorRefState).
        labelNext("tell") foreach {
        case (null, (_, _)) ⇒
          throw new IllegalStateException("toActorIndirect(" + actorRefState + "): actorRef is not initialized.")
        case (actor, (null, msg)) ⇒
          throw new IllegalStateException("toActorIndirect(" + actorRefState + "): self is not initialized.")
        case (actor, (selfRef: ActorRef, msg)) ⇒
          actor.tell(msg, selfRef)
        //          case msg ⇒
        //            throw new IllegalStateException("toActorIndirect(" + actorRefState + "): Impossible case:" + msg)
      }
    }
  }

}

/** Creates subsystem that is built around some actor.
  * The subsystem should be used inside ActorComponent.
  * The data that appears on input contact is sent to
  * the actor and the answer is caught and sent to output.
  *
  * This adapter can be used to incorporate existing Actor implementation
  * inside synapse-grid. However it is not recommended as general mechanism.
  * If one needs to use ordinary actors, why not created them?
  * If
  */
class ChildActorAdapterSnippet[TInput, TOutput](
                                                 name: String,
                                                 input: Contact[TInput],
                                                 outputContact: Contact[TOutput])(factory: ActorRef ⇒ Actor)
  extends ActorSystemBuilder with SystemBuilderWithLogging {

  inputs(ContextInput, NonSignalWithSenderInput, input)
  outputs(outputContact)

  setSystemName(name + "System")

  val actorRef = state[ActorRef](name, Actor.noSender)

  ContextInput debug() labelNext s"create actor $name" map {
    case contextRef ⇒
      contextRef.actorOf(Props(factory(contextRef.self)), name)
  } saveTo actorRef

  new ImplRichContactActor(input).toActorIndirect(actorRef)


  NonSignalWithSenderInput delay 2 zipWithState
    actorRef labelNext "sender == actor?" flatMap {
    case (actor, (senderRef, msg)) =>
      if (actor == senderRef)
        Seq(msg.asInstanceOf[TOutput])
      else
        Seq()
  } directly outputContact
}
