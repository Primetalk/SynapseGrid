package ru.primetalk.synapse.akka

import ru.primetalk.synapse.core._
import akka.actor._
import ru.primetalk.synapse.akka.SpecialActorContacts.{ContextInput, SenderInput}
/**
 * An extension for SystemBuilder that adds a few contacts and states
 * that are typically necessary to integrate with other Akka actors.
 *
 * @author zhizhelev, 20.07.15.
 */
trait ActorSystemBuilderExt {
  implicit val ActorSystemBuilderExtensionId = new SystemBuilderExtensionId[ActorSystemBuilderExtension](new ActorSystemBuilderExtension(_))


  /** Basic builder that defines a few helpers for constructing actor-held systems. */
  class ActorSystemBuilderExtension(val sb: SystemBuilder) extends SystemBuilderExtension {
    implicit val sb1 = sb
    lazy val sender = {
      val sender1 = state[ActorRef]("sender", akka.actor.Actor.noSender)

      sb.inputs(SenderInput)
      SenderInput.saveTo(sender1)
      sender1
    }

    lazy val context = {
      val context1 = state[ActorContext]("context", null)
      sb.inputs(ContextInput)
      ContextInput.saveTo(context1)
      context1
    }

    lazy val self = {
      val self1 = state[ActorRef]("self", akka.actor.Actor.noSender)

      sb.inputs(ContextInput)
      ContextInput map(_.self, "_.self") saveTo self1
      self1
    }
    lazy val SelfInput = {
      val SelfInput1 = contact[ActorRef]("SelfInput")

      sb.inputs(ContextInput)
      ContextInput -> SelfInput1 map(_.self, "_.self")
      SelfInput1
    }


    implicit class ImplRichContactUnzipperToActor[T](c: Contact[(ActorRef, T)]) {
      //extends ImplRichContact[(ActorRef, T)](c) {
      def tellToActor(actor: ActorRef, name: String = "") = {
        c foreach(p => actor.tell(p._2, p._1), nextLabel(name, "tellToActor(" + actor.path + ")"))
      }

    }

    class ImplRichContactActor[T](c: Contact[T]) {
      /** Send a message to actorRef in the given state.*/
      def toActorIndirect(actorRefState: StateHandle[ActorRef], name: String = "") = {
        c.from(self).
          labelNext("to @" + actorRefState).
          zipWithState(actorRefState).
          labelNext("tell") foreach {
          case (null, (_, _)) =>
            throw new IllegalStateException("toActorIndirect(" + actorRefState + "): actorRef is not initialized.")
          case (actor, (null, msg)) =>
            throw new IllegalStateException("toActorIndirect(" + actorRefState + "): self is not initialized.")
          case (actor, (selfRef: ActorRef, msg)) =>
            actor.tell(msg, selfRef)
          //          case msg =>
          //            throw new IllegalStateException("toActorIndirect(" + actorRefState + "): Impossible case:" + msg)
        }
      }
      /** Send a message from the previous contact to the given actor from self. */
      def tellToActorFromSelf(actor: ActorRef, name: String = "") = {
        c from self tellToActor(actor, name)
      }
    }

  }

}
