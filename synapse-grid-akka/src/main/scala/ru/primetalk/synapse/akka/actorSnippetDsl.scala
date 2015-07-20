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

import ru.primetalk.synapse.slf4j._




trait ActorSnippetDsl {
  /**
   * Create subsystem for the child actor.
   *
   * @param factory create actor using the supplied parent reference.
   */
  def childActorAdapterSnippet[TInput, TOutput](name: String,
                                                input: Contact[TInput], outputContact: Contact[TOutput])(factory: ActorRef ⇒ Actor)(implicit sb:SystemBuilder): StaticSystem = {
    sb.inputs(NonSignalWithSenderInput)
    val innerSb = new SystemBuilderC(name)
    new ChildActorAdapterSnippet(name, input, outputContact)(factory)(innerSb)
    innerSb.toStaticSystem
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
                                                 outputContact: Contact[TOutput])(factory: ActorRef ⇒ Actor)(implicit sb:SystemBuilder){

//  val sbSlf4j = sb.extend(SystemBuilderLoggingExtensionId)
val akkaExt = sb.extend(ActorSystemBuilderExtensionId)
  sb.inputs(ContextInput, NonSignalWithSenderInput, input)
  sb.outputs(outputContact)

  setSystemName(name + "System")

  val actorRef = state[ActorRef](name, Actor.noSender)

  ContextInput debug() labelNext s"create actor $name" map {
    case contextRef ⇒
      contextRef.actorOf(Props(factory(contextRef.self)), name)
  } saveTo actorRef

  new akkaExt.ImplRichContactActor(input).toActorIndirect(actorRef)

  NonSignalWithSenderInput delay 2 zipWithState
    actorRef labelNext "sender == actor?" flatMap {
    case (actor, (senderRef, msg)) =>
      if (actor == senderRef)
        Seq(msg.asInstanceOf[TOutput])
      else
        Seq()
  } directly outputContact
}
