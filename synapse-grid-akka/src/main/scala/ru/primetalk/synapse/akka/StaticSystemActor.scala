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
package ru.primetalk.synapse.akka

import ru.primetalk.synapse.core._
import akka.event.{LoggingReceive, Logging}
import org.slf4j.MDC
import ru.primetalk.synapse.akka.SpecialActorContacts._
import akka.actor._
import ru.primetalk.synapse.core.Signal
import ru.primetalk.synapse.akka.SpecialActorContacts.InitCompleted
import SystemConvertingSupport._
import ru.primetalk.synapse.core

/** Escalates all exceptions to upper level. This actor is an appropriate default for
  * in-channel actors. */
trait EscalatingActor extends Actor {
  override val supervisorStrategy =
    defaultSupervisorStrategy
}

/** An actor that contains a Static system and processes signals */
trait AbstractStaticSystemActor extends Actor {

  val systemPath: List[String]
  val system: StaticSystem
  val outputFun: Option[InternalSignals => Any]
  /** None for top level system */
  val parentSystemRef: Option[ActorRef]

  protected val log = Logging(context.system, this)
  var systemState = system.s0
  val processor: TotalTrellisProducer = createSelfTrellisProducer


  /** Creates a trellis producer for the system itself. */
  protected
  def createSelfTrellisProducer: TotalTrellisProducer

  val processSignals = createSignalProcessor

  protected def createSignalProcessor: (List[Signal[_]]) => Unit = {
    if (system.inputContacts.contains(SenderInput)) // the check is done at the beginning.
      (ls: List[Signal[_]]) ⇒
        innerProcessSignals(Signal(SenderInput, sender) :: ls)
    else
      innerProcessSignals _
  }

  protected def innerProcessSignals(ls: List[Signal[_]]) {
    MDC.put("akkaSource", "" + self.path)
    val results = ls.flatMap {
      signal: Signal[_] =>
        val res = processor(systemState, signal)
        systemState = systemState ++ res._1
        res._2
    }
    if (!results.isEmpty) {
      val sigs = InternalSignals(systemPath, results)
      outputFun.foreach(_(sigs))
      parentSystemRef.foreach(_ ! sigs)
    }
  }

  def receive = LoggingReceive {
    case s@Signal(_, _) ⇒
      processSignals(s :: Nil)
    case InternalSignals(path, signals) =>
      processSignals(signals.map(signal =>
        (signal.asInstanceOf[Signal[Any]] /: path)((s, name: String) =>
          Signal(SubsystemSpecialContact: Contact[SubsystemDirectSignal], SubsystemDirectSignal(name, s)).asInstanceOf[Signal[Any]]
        )
      ))
    case nonSignalMessage ⇒
      val s = Signal(NonSignalWithSenderInput, (sender, nonSignalMessage))
      processSignals(s :: Nil)
  }

  override def preStart() {
    if (system.inputContacts.contains(ContextInput))
      processSignals(Signal(ContextInput, context) :: Nil)
    if (system.inputContacts.contains(PreStartInput))
      processSignals(Signal(PreStartInput, context) :: Nil)
    parentSystemRef.foreach(_ ! InitCompleted(self))
  }

  override def postStop() {
    if (system.inputContacts.contains(PostStopInput))
      processSignals(Signal(PostStopInput, PostStop) :: Nil)
  }

}

/**
 * Actor that corresponds to the given static system. It will work according to
 * the schema of the system.
 * If there are ActorInnerSubsystem-s within the system, then they will become children actors of this one.
 *
 * @param systemPath the list of intermediate systems from parent actor to the system of this actor
 */
class StaticSystemActor(override val systemPath: core.SystemPath,
                        override val system: StaticSystem,
                        override val outputFun: Option[InternalSignals => Any] = None,
                        override val supervisorStrategy: SupervisorStrategy) extends AbstractStaticSystemActor {

  val parentSystemRef: Option[ActorRef] = Option(context.parent)

  /** Creates a trellis producer for the system itself. */
  protected
  override
  def createSelfTrellisProducer: TotalTrellisProducer = {
    StaticSystemActor.toSingleSignalProcessor(context, self)(systemPath, system)
  }


}


object StaticSystemActor {

  type ActorRefGetter = (
    /*path: */ core.SystemPath,
    /*subsystem: */ StaticSystem,
    /*supervisorStrategy: */ SupervisorStrategy,
    /*systemName: */ String) => ActorRef


  //  override val systemPath: core.SystemPath,
  //  override val system: StaticSystem,
  //  override val outputFun: Option[InternalSignals => Any] = None,
  //  override val supervisorStrategy: SupervisorStrategy

  def toSingleSignalProcessor(actorRefFactory: ActorRefFactory,
                              self: ActorRef = Actor.noSender
                               )(
                               path: core.SystemPath,
                               system: StaticSystem): TotalTrellisProducer = {
    val actorInnerSubsystemConverter: ComponentDescriptorConverter = {
      case ComponentDescriptor(ActorInnerSubsystem(subsystem, supervisorStrategy), path1, _) =>
        val actorRef = actorRefFactory.actorOf(Props(
          new StaticSystemActor(path1, subsystem, None, supervisorStrategy)),
          subsystem.name)
        RuntimeComponentMultiState(subsystem.name, List(), (context: Context, signal) => {
          actorRef.tell(signal, self)
          (context, List())
        })
    }


    val converter = {
      val c =
        SystemConverting.componentToSignalProcessor2(_.toTotalTrellisProducer,
          RuntimeComponent.linkToRuntimeComponent,
          actorInnerSubsystemConverter)
      //      c += actorInnerSubsystemConverter
      c
    }
    val rs = SystemConverting.systemToRuntimeSystem(path, system, converter, system.outputContacts)
    val proc = rs.toTotalTrellisProducer
    proc
  }

  /** Converts top level system to top level actor. */
  def toActorTree(actorRefFactory: ActorRefFactory,
                  supervisorStrategy: SupervisorStrategy =
                  defaultSupervisorStrategy)(path: List[String], system: StaticSystem, outputFun: Option[InternalSignals => Any] = None): ActorRef =
    actorRefFactory.actorOf(Props(
      new StaticSystemActor(path, system, outputFun, supervisorStrategy)),
      system.name)

}
