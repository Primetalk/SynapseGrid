package ru.primetalk.synapse.akka  


import akka.actor.{Props, Actor, ActorContext, ActorRef}


import ru.primetalk.synapse.core._

class ActorSystemBuilderOps(implicit sb:BasicSystemBuilder) {
	def addActorSubsystem[T](subsystem: T)(implicit ev:T=>StaticSystem):T = {
		sb.addComponent(new ActorInnerSubsystem(subsystem))
		subsystem
	}

	/**
	 * Create subsystem for the child actor.
	 *
	 * @param factory create actor using the supplied parent reference.
	 */
	def childActorAdapterSnippet[TInput, TOutput](name: String,
		input: Contact[TInput], outputContact: Contact[TOutput])(factory: ActorRef ⇒ Actor): StaticSystem = {
		sb.inputs(SpecialActorContacts.NonSignalWithSenderInput)
		new ChildActorAdapterSnippet(name, input, outputContact)(factory).toStaticSystem
	}	

}
