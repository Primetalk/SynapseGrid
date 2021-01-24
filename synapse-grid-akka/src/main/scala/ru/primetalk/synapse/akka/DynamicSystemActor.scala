///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * ${PROJECT_NAME}
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 *
 * Created: 01.08.13, zhizhelev
 */
package ru.primetalk.synapse.akka

import akka.event.{Logging, LoggingAdapter, LoggingReceive}
import ru.primetalk.synapse.akka.SpecialActorContacts._
import ru.primetalk.synapse.akka.impl.EscalatingActor
import ru.primetalk.synapse.core.syntax._
import ru.primetalk.synapse.core.syntax.given
import ru.primetalk.synapse.akka.SpecialActorContacts.InitCompleted
import org.slf4j.MDC

import scala.concurrent.duration._
import scala.language.postfixOps
import akka.actor.actorRef2Scala

/**
 * The actor is an envelope around an arbitrary dynamic system.
 * @param path the path of the system.
 * @param system the system itself
 */
class DynamicSystemActor(path: List[String], system: DynamicSystem) extends EscalatingActor {

  require(path.nonEmpty,"The system's path should  not be empty")
  val log: LoggingAdapter = Logging(context.system, this)

  private def innerProcessSignals(ls: List[Signal0]): Unit = {
    MDC.put("akkaSource", "" + self.path)
    val results = ls.flatMap(system.receive)
    if results.nonEmpty then
      context.parent ! InternalSignalsDist(path, results.map(system.index.convertSignalToSignalDist))
  }

  val processSignals: List[Signal0] => Unit =
    if system.inputContacts.contains(SenderInput) then // the check is done at the beginning.
      (ls: List[Signal0]) =>
        innerProcessSignals(Signal(SenderInput, sender()) :: ls)
    else
      innerProcessSignals

  private object Tick

  if system.inputContacts.contains(CurrentTimeMsInput) then
    context.system.scheduler.schedule(0 milliseconds, 10 milliseconds, // scheduler has 100ms precision. Thus it will be called only once in 100 ms. (tick-interval)
      self, Tick)(context.dispatcher)

  def receive: Receive = LoggingReceive {
    case s@Signal(_, _) =>
      processSignals(s :: Nil)
    case InternalSignalsDist(systemPath, signalsDist) =>

      processSignals(
        systemPath.reverse match {
          case Nil =>
            signalsDist.map(sd => system.index(sd))
          case _ =>
            throw new NotImplementedError("Processing signals for innermost systems is not implemented " + systemPath)
        }
      )
    case nonSignalMessage =>
      val s = Signal(NonSignalWithSenderInput, (sender(), nonSignalMessage))
      processSignals(s :: Nil)
  }

  override def preStart(): Unit = {
    if system.inputContacts.contains(ContextInput) then
      processSignals(Signal(ContextInput, context) :: Nil)
    if system.inputContacts.contains(PreStartInput) then
      processSignals(Signal(PreStartInput, context) :: Nil)
    context.parent ! InitCompleted(self)
  }

  override def postStop(): Unit = {
    if system.inputContacts.contains(PostStopInput) then
      processSignals(Signal(PostStopInput, PostStop) :: Nil)
  }
}
