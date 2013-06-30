///////////////////////////////////////////////////////////////
// Речевой портал                                            //
// © ООО «Праймтолк», 2011-2013                              //
// Авторы: Жижелев А.А., Нехаев А.Р., Пешков И.В., Попов П.А.//
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * Speech portal
 * © Primetalk Ltd., 2011-2013.
 * Authors: Zhizhelev A., Nehaev A., Peshkov I., Popov P.
 * All rights reserved.
 * Created: 13.02.2013
 */
package ru.primetalk.sinapse.akka


import ru.primetalk.sinapse.core._
import akka.actor._
import akka.actor.SupervisorStrategy.Escalate
import akka.event.{LoggingReceive, Logging}
import ru.primetalk.sinapse.core.Signal
import ru.primetalk.sinapse.core.DynamicSystem
import akka.actor.AllForOneStrategy
import org.slf4j.MDC
import scala.concurrent.duration._

///** Signals from out*/
//case class Signals(list: List[Signal[_]])
/** signals from subsystems.*/
case class InternalSignals(list: List[Signal[_]])
/**
 * Helper method that sends signal to the given actor
 */
trait ToActorUtilityT {

	/** Sends the data directly. */
	def signalToActor(actorRef: ActorRef)(signal: Signal[_]) {
    actorRef ! signal
  }
			
	/** Sends the data directly. */
	def signalDataToActor(actorRef: ActorRef)(signal: Signal[_]) {
    actorRef ! signal.data
  }

	/** Sends a signal with data. */
	def dataToActorContact[T](actorRef: ActorRef, contact: Contact[T])(data: T) {
    actorRef ! Signal(contact, data)
  }
	implicit class ActorWithContact[T](ac : (ActorRef, Contact[T])) {
		val (actorRef, contact) = ac
		def !(data : T)(implicit sender:ActorRef = Actor.noSender) {
			actorRef ! Signal(contact, data)
		}
	}
}
/** Escalates all exceptions to upper level. This actor is an appropriate default for 
 *  in-channel actors.*/
trait EscalatingActor extends Actor {
	override val supervisorStrategy = 
		AllForOneStrategy(){
			case _:Throwable=>Escalate
		}
}
object SpecialActorContacts {
	/** A special contact that will get sender if inputs contain it.*/
	object SenderInput extends Contact[ActorRef]
	/** This contact receives Actor's context in the very beginning.*/
	object ContextInput extends Contact[ActorContext]
	object PreStartInput extends Contact[Any]
	
	/**
	  * A special contact that will get external messages that are not represented with signals.
	  *  It also receives the sender of the message.
	  */
	object NonSignalWithSenderInput extends Contact[(ActorRef, Any)]
	/** The system that wants current time can put this contact in inputs and will obtain ticks with current time.*/
	object CurrentTimeMsInput extends Contact[Long]
	sealed trait LifeControlCommand
	/** Should prepare a system for termination. 
	 *  After processing this signal all further messages won't go into the system. 
	 *  They should be ignored if some of them do.
	 *  
	 *  Anyway, the system should release resources and it should dispatch control signal further to children.
	 *  And the system itself should produce neither signals anymore. 
	 *  
	 *  When getting this signal the system should answer with LifeControlCommandReport.
	 *  */
	case object PreDie extends LifeControlCommand
	case object Die extends LifeControlCommand
	case object Reset extends LifeControlCommand

	case object PostStop extends LifeControlCommand

	/** */
	case class LifeControlCommandReport (cmd:LifeControlCommand, failureDescription:Option[String])
	
	sealed trait InitializationReport
	/** Special actor message to send to parent when the actor has been initialized.*/
	case class InitCompleted(actor:ActorRef) extends InitializationReport
	/**
	  * If actor couldn't initialize itself due to some exception.
	  * There can also be timeout for initialization. In this case 
	  * The actor can receive `stop` after InitSuccess
	  */
	case class InitFailed(actor:ActorRef, cause : Throwable) extends InitializationReport
	
	sealed trait LifeCycleReport
	case object LifeControl extends Contact[LifeControlCommand]
	case object PreDieControl extends Contact[LifeControlCommand]
	case object LifeControlReport extends Contact[LifeControlCommandReport]
	
	case object PostStopInput extends Contact[LifeControlCommand]
}
import SpecialActorContacts._
class DynamicSystemActor(system:DynamicSystem) extends EscalatingActor {
	val log = Logging(context.system, this)
	val processSignals =
		if (system.inputContacts.contains(SenderInput)) // the check is done at the beginning.
			(ls : List[Signal[_]]) ⇒ {
				MDC.put("akkaSource", ""+self.path)
				val signals = Signal(SenderInput, sender) :: ls
				val results = signals.flatMap(system.receive(_))
				if(!results.isEmpty)
					context.parent ! InternalSignals(results)
			}
		else 
			(ls : List[Signal[_]]) ⇒ {
				val results = ls.flatMap(system.receive(_))
				if(!results.isEmpty)
					context.parent ! InternalSignals(results)
			}
	private object Tick

	if(system.inputContacts.contains(CurrentTimeMsInput))
		context.system.scheduler.schedule(0 milliseconds, 10 milliseconds, // scheduler has 100ms precision. Thus it will be called only once in 100 ms. (tick-interval)
	  		self, Tick)(context.dispatcher)
  def receive = LoggingReceive {
		case s @ Signal(_, _) ⇒
			processSignals(s :: Nil)
		case Tick ⇒
			processSignals(Signal(CurrentTimeMsInput, System.currentTimeMillis) :: Nil)
		case nonSignalMessage ⇒
			val s = Signal(NonSignalWithSenderInput, (sender, nonSignalMessage))
			processSignals(s :: Nil)
	}
	override def preStart() {
		if(system.inputContacts.contains(ContextInput))
			processSignals( Signal(ContextInput, context)::Nil)
		if(system.inputContacts.contains(PreStartInput)) 
			processSignals( Signal(PreStartInput, context)::Nil)
		context.parent ! InitCompleted(self)
	}
	override def postStop() {
		if(system.inputContacts.contains(PostStopInput))
			processSignals( Signal(PostStopInput, PostStop)::Nil)
	}
}



///** Sends raw data from input signals to the actor. All input contacts are treated equivalently.*/
//case class SimpleActorAdapter(
//		inputContacts: Set[Contact[_]],
//		name: String,
//		/** This actor can send signals to sender (reply). However they are not restricted by output contacts.*/
//		simpleActorRef:ActorRef) extends OuterSystem {
//	val outputContacts = Set[Contact[_]]()
//}
///** An actor that converts any raw data to signal and forwards it to contactsActor */
//class ActorSendingToContact[T](contactsActor : ActorRef, contact : Contact[T]) extends Actor {
//	def receive = {
//		case s @ Signal(c, d) if c == contact ⇒ 
//			contactsActor forward Signal(contact, d.asInstanceOf[T])
//		case msg ⇒ 
//			contactsActor forward Signal(contact, msg.asInstanceOf[T])
//	}
//}
//case class MultiOutputAdapter(
//		inputContacts: Set[Contact[_]],
//		outputContacts: Set[Contact[_]],
//		name: String)(
//		/** the function from some single signal on input contact to any number of output contacts.*/
//		val multiOutputAlgorithm:PartialFunction[Any,List[Signal[_]]]) extends OuterSystem
//case class ActorBuilder(
//	inputContacts : Set[Contact[_]],
//	outputContacts : Set[Contact[_]],
//	name : String)
//	(val actorFactory : (List[Signal[_]] ⇒ Any) ⇒ Actor) extends OuterSystem
//case class ArbitraryChildActorAdapter(
//	inputContacts : Set[Contact[_]],
//	outputContacts : Set[Contact[_]],
//	name : String,
//	/** the factory gets the ActorRef to parent (event hub).
//	 *   The Actor behind ActorRef only handles Signals. Thus a converter may be necessary. */ 
//	actorFactory : (ActorRef) ⇒ Actor) extends OuterSystem
///**
//  * This declarations leads to creation of intermediate actor that converts
//  *  input signals to raw data and then sends it to the actorRef. The replies of
//  *  that actor should not be signals. They come back to the intermediate actor
//  *  which applies mapping function replyMap to convert them to Signals.
//  *  Converts replies of the actor to contacts.
//  */
//case class ActorRefAdapter(
//	inputContacts : Set[Contact[Any]],
//	outputContacts : Set[Contact[Any]],
//	/** signals to input contacts are preprocessed before this map. 
//	 *  This function should convert other messages to signals, if it can. */
//	replyMap : PartialFunction[Any, Signal[_]], 
//	name : String, 
//	actorRef : ActorRef) extends OuterSystem
//case class FunctionalSystem(
//		inputContacts: Set[Contact[Any]],
//		outputContacts: Set[Contact[Any]],
//		name: String,
//		actorFactory:(List[Signal[_]]=>Any) => Actor) extends OuterSystem
//		case ActorRefAdapter(inputContacts, _, replyMap, _, actorRef) =>
//			new Actor {
//				val processRequests:Receive = {
//					case Signal(c, d) if inputContacts.contains(c) ⇒ actorRef ! d
//				}
//				val processAnswers:Receive = replyMap.andThen(s => parent ! s)
//				def receive = processRequests orElse processAnswers
//			}
