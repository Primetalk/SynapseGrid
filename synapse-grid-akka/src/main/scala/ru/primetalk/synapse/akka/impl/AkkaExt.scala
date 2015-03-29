package ru.primetalk.synapse.akka.impl

import akka.actor.{ActorContext, ActorRef}
import ru.primetalk.synapse.akka.SpecialActorContacts.{ContextInput, SenderInput}
import ru.primetalk.synapse.core._

import scala.language.implicitConversions
/**
 * Extension that allows special akka-contacts to be used.
 * @author zhizhelev, 25.03.15.
 */
trait AkkaExt {

  class AkkaSystemBuilderExtension(val sb: BasicSystemBuilder) extends SystemBuilderExtension {

    lazy val sender = {
      @inline implicit def sb1: BasicSystemBuilder = sb
      val sender1 = sb.state[ActorRef]("sender", _root_.akka.actor.Actor.noSender)

      sb.inputs(SenderInput)
      SenderInput.saveTo(sender1)
      sender1
    }

    lazy val context = {
      implicit def sb1: BasicSystemBuilder = sb
      val context1 = sb.state[ActorContext]("context", null)
      sb.inputs(ContextInput)
      ContextInput.saveTo(context1)
      context1
    }

    lazy val self = {
      @inline implicit def sb1: BasicSystemBuilder = sb
      val self1 = sb.state[ActorRef]("self", _root_.akka.actor.Actor.noSender)

      sb.inputs(ContextInput)
      ContextInput map(_.self, "_.self") saveTo self1
      self1
    }
    lazy val SelfInput = {
      @inline implicit def sb1: BasicSystemBuilder = sb
      val SelfInput1 = contact[ActorRef]("SelfInput")

      sb.inputs(ContextInput)
      ContextInput -> SelfInput1 map(_.self, "_.self")
      SelfInput1
    }


    implicit class ImplRichContactUnzipperToActor[T](c: Contact[(ActorRef, T)]) {
      //extends ImplRichContact[(ActorRef, T)](c) {
      def tellToActor(actor: ActorRef, name: String = "") = {
        @inline implicit def sb1: BasicSystemBuilder = sb
        c foreach(p => actor.tell(p._2, p._1), sb.nextLabel(name, "tellToActor(" + actor.path + ")"))
      }

      def tellToActorFromSelf(actor: ActorRef, name: String = "") = {
        @inline implicit def sb1: BasicSystemBuilder = sb
        c from self tellToActor(actor, name)
      }
    }

    class ImplRichContactActor[T](c: Contact[T]) {
      @inline implicit def sb1: BasicSystemBuilder = sb
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

  implicit val akkaExtensionId = new SystemBuilderExtensionId(new AkkaSystemBuilderExtension(_))

//  implicit def akkaExtension(implicit sb:BasicSystemBuilder):AkkaSystemBuilderExtension = sb.extend(akkaExtensionId)
}
