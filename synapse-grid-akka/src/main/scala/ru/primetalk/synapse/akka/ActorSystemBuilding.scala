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

import ru.primetalk.synapse.core.{StaticSystem, StateHandle, SystemBuilder, Contact, Component}
import akka.actor.{Props, Actor, ActorContext, ActorRef}
import ru.primetalk.synapse.akka.SpecialActorContacts.{NonSignalWithSenderInput, ContextInput, SenderInput}
import ru.primetalk.synapse.slf4j.SystemBuilderWithLogging

/**
 * For an ActorInnerSubsystem an actor will be constructed. The state will reside in that actor.
 * @author А.Жижелев
 *
 */
case class ActorInnerSubsystem(subsystem: StaticSystem) extends Component {
	def name = subsystem.name
	val inputContacts = subsystem.inputContacts
	val outputContacts = subsystem.outputContacts
}
trait ActorContainerBuilder extends SystemBuilder {
	def addActorSubsystem(subsystem: StaticSystem) {
		addComponent(new ActorInnerSubsystem(subsystem))
	}

	/**
	 * Create subsystem for the child actor.
	 *
	 * @param factory create actor using the supplied parent reference.
	 */
	def childActorAdapterSnippet[TInput, TOutput](name: String,
		input: Contact[TInput], outputContact: Contact[TOutput])(factory: ActorRef ⇒ Actor): StaticSystem = {
		NonSignalWithSenderInput.input
		new ChildActorAdapterSnippet(name, input, outputContact)(factory).toStaticSystem
	}
}
/** Basic builder that defines a few helpers for constructing actor-held systems. */
trait ActorSystemBuilder extends ActorContainerBuilder {
	inputs(SenderInput, ContextInput)
	val sender = state[ActorRef]("sender", akka.actor.Actor.noSender)
	labels("saveTo("+sender+")")
	SenderInput.saveTo(sender)

	val context = state[ActorContext]("context", null)
	ContextInput.saveTo(context)

	val self = state[ActorRef]("self", akka.actor.Actor.noSender)
	val SelfInput = contact[ActorRef]("SelfInput")
	ContextInput -> SelfInput map(_.self, "_.self") saveTo self
	
	implicit class ImplRichContactUnzipperToActor[T](c : Contact[(ActorRef, T)]) {//extends ImplRichContact[(ActorRef, T)](c) {
		def tellToActor(actor:ActorRef, name:String = "") = {
			c foreach (p => actor.tell(p._2,p._1), nextLabel(name,"tellToActor("+actor.path+")"))
		}
		def tellToActorFromSelf (actor:ActorRef, name:String = "") = {
			c from self tellToActor (actor, name)
		}
	}
	class ImplRichContactActor[T](c : Contact[ T]) extends ImplRichContact[T](c) {
		def toActorIndirect (actorRefState:StateHandle[ActorRef], name:String = "") = {
			from(self).
			labelNext ("to @" + actorRefState).
			zipWithState(actorRefState). 
				labelNext ("tell") foreach {
          case (null, (_, _)) ⇒
            throw new IllegalStateException("toActorIndirect(" + actorRefState + "): actorRef is not initialized.")
          case (actor, (null, msg)) ⇒
            throw new IllegalStateException("toActorIndirect(" + actorRefState + "): self is not initialized.")
          case (actor, (selfRef:ActorRef, msg)) ⇒
            actor.tell(msg, selfRef)
//          case msg ⇒
//            throw new IllegalStateException("toActorIndirect(" + actorRefState + "): Impossible case:" + msg)
        }
		}
	}
	
}

/** Creates subsystem that is built around some actor.*/
class ChildActorAdapterSnippet[TInput, TOutput](
		name : String, 
		input:Contact[TInput], 
		outputContact:Contact[TOutput])(factory : ActorRef ⇒ Actor) extends ActorSystemBuilder with SystemBuilderWithLogging {
  inputs(ContextInput, NonSignalWithSenderInput, input)
  outputs(outputContact)

	setSystemName(name+"System")
	val actorRef  = state[ActorRef](name, Actor.noSender)
	ContextInput debug() labelNext s"create actor $name" map {
		case contextRef ⇒
      contextRef.actorOf(Props(() ⇒ factory(contextRef.self)), name)
	} saveTo actorRef
	
	new ImplRichContactActor(input). toActorIndirect(actorRef)
	
	
	NonSignalWithSenderInput delay 2 zipWithState
    actorRef labelNext "sender == actor?" flatMap {
		case (actor, (senderRef, msg)) =>
			if(actor == senderRef)
				Seq(msg.asInstanceOf[TOutput])
			else
				Seq()
	} directly outputContact
}
