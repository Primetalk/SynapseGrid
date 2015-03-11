package ru.primetalk.synapse

import _root_.akka.actor._
import _root_.akka.actor.AllForOneStrategy
import ru.primetalk.synapse.akka.SpecialActorContacts.{ContextInput, SenderInput}
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

  implicit class RichStaticSystemSystem(s: StaticSystem) {
    /**
     * The main DSL function to convert a StaticSystem to an actor tree.
     * Returns top level actor.
     * @param threadSafeOutputFun - a function that will receive output signals of the actor.
     *                            Should be thread safe!!!
     *                            May be omitted. In the latter case the output signals
     *                            are ignored.*/
    def toActorTree(threadSafeOutputFun: Option[InternalSignalsDist => Any] = None)
                   (implicit actorRefFactory: ActorRefFactory): ActorRef =
      StaticSystemActor.toActorTree(actorRefFactory)(List(), s, threadSafeOutputFun)

    def toActorTree(implicit actorRefFactory: ActorRefFactory): ActorRef =
      toActorTree(None)(actorRefFactory)
  }

  implicit def toActorSystemBuilder[T <: BasicSystemBuilder](sb: T):ActorSystemBuilderOps = new ActorSystemBuilderOps()(sb)

  val defaultSupervisorStrategy: SupervisorStrategy = AllForOneStrategy() {
    case _: Throwable => SupervisorStrategy.Escalate
  }

  //  implicit def signalToSignalDist[T](s:Signal[T])(implicit context:ActorContext):SignalDist ={
  //    val id = ContactsMapExtension(context.system).getContactId(s.contact)
  //    SignalDist(id, s.data.asInstanceOf[java.lang.Object])
  //  }
  //
  //  implicit def signalDistToSignal(s:SignalDist)(implicit context:ActorContext):Signal[_] ={
  //    val c = ContactsMapExtension(context.system).getContact(s.contactId).asInstanceOf[Contact[Any]]
  //    Signal(c, s.data)//.asInstanceOf[Signal[_]]
  //  }
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

  val akkaExtensionId = new SystemBuilderExtensionId(new AkkaSystemBuilderExtension(_))

  implicit def akkaExtension(implicit sb:BasicSystemBuilder):AkkaSystemBuilderExtension = sb.extend(akkaExtensionId)
}
