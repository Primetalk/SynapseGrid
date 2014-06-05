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

import akka.event.{LoggingReceive, Logging}
import ru.primetalk.synapse.akka.SpecialActorContacts._
import ru.primetalk.synapse.core.Signal
import ru.primetalk.synapse.akka.SpecialActorContacts.InitCompleted
import ru.primetalk.synapse.core.DynamicSystem
import org.slf4j.MDC
import scala.concurrent.duration._
import scala.language.postfixOps

/**
 * The actor is an envelope around an arbitrary dynamic system.
 * @param path the path of the system.
 * @param system the system itself
 */
class DynamicSystemActor(path: List[String], system: DynamicSystem) extends EscalatingActor {

  val log = Logging(context.system, this)

  private def innerProcessSignals(ls: List[Signal[_]]) {
    MDC.put("akkaSource", "" + self.path)
    val results = ls.flatMap(system.receive)
    if (!results.isEmpty)
      context.parent ! InternalSignalsDist(path, results.map(system.index.convertSignalToSignalDist))
  }

  val processSignals =
    if (system.inputContacts.contains(SenderInput)) // the check is done at the beginning.
      (ls: List[Signal[_]]) ⇒
        innerProcessSignals(Signal(SenderInput, sender()) :: ls)
    else
      innerProcessSignals _

  private object Tick

  if (system.inputContacts.contains(CurrentTimeMsInput))
    context.system.scheduler.schedule(0 milliseconds, 10 milliseconds, // scheduler has 100ms precision. Thus it will be called only once in 100 ms. (tick-interval)
      self, Tick)(context.dispatcher)

  def receive = LoggingReceive {
    case s@Signal(_, _) ⇒
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
    case nonSignalMessage ⇒
      val s = Signal(NonSignalWithSenderInput, (sender(), nonSignalMessage))
      processSignals(s :: Nil)
  }

  override def preStart() {
    if (system.inputContacts.contains(ContextInput))
      processSignals(Signal(ContextInput, context) :: Nil)
    if (system.inputContacts.contains(PreStartInput))
      processSignals(Signal(PreStartInput, context) :: Nil)
    context.parent ! InitCompleted(self)
  }

  override def postStop() {
    if (system.inputContacts.contains(PostStopInput))
      processSignals(Signal(PostStopInput, PostStop) :: Nil)
  }
}
