/////////////////////////////////////////////////////
// Речевой портал                                  //
// © ООО «Праймтолк», 2011-2013                    //
// Авторы: Жижелев А.А., Нехаев А.Р., Попов П.А.   //
// Все права принадлежат компании ООО «Праймтолк». //
/////////////////////////////////////////////////////
/**
 * Speech portal
 * © Primetalk Ltd., 2011-2013.
 * Authors: Zhizhelev A., Nehaev A., Popov P.
 * All rights reserved.
 * Created: 13.02.2013
 */
package ru.primetalk.synapse.akka


import akka.actor._
import akka.actor.SupervisorStrategy.Escalate
import akka.event.{LoggingReceive, Logging}
import ru.primetalk.synapse.core.Signal
import ru.primetalk.synapse.core.DynamicSystem
import akka.actor.AllForOneStrategy
import org.slf4j.MDC
import scala.concurrent.duration._
import scala.language.postfixOps
///** Signals from external systems.*/
// @deprecated("Signals should come one by one", "01.07.2013")
//case class Signals(list: List[Signal[_]])
/** signals from subsystems.*/
case class InternalSignals(list: List[Signal[_]])

/** Escalates all exceptions to upper level. This actor is an appropriate default for 
 *  in-channel actors.*/
trait EscalatingActor extends Actor {
	override val supervisorStrategy = 
		AllForOneStrategy(){
			case _:Throwable=>Escalate
		}
}

import SpecialActorContacts._
class DynamicSystemActor(system:DynamicSystem) extends EscalatingActor {
	val log = Logging(context.system, this)
	private def innerProcessSignals(ls : List[Signal[_]]){
		MDC.put("akkaSource", ""+self.path)
		val results = ls.flatMap(system.receive)
		if(!results.isEmpty)
			context.parent ! InternalSignals(results)
	}
	val processSignals =
		if (system.inputContacts.contains(SenderInput)) // the check is done at the beginning.
			(ls : List[Signal[_]]) ⇒
				innerProcessSignals (Signal(SenderInput, sender) :: ls)
		else
			innerProcessSignals(_)

	private object Tick

	if(system.inputContacts.contains(CurrentTimeMsInput))
		context.system.scheduler.schedule(0 milliseconds, 10 milliseconds, // scheduler has 100ms precision. Thus it will be called only once in 100 ms. (tick-interval)
	  		self, Tick)(context.dispatcher)
  def receive = LoggingReceive {
		case s @ Signal(_, _) ⇒
			processSignals(s :: Nil)
    case InternalSignals(signals) =>
      processSignals(signals)
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


