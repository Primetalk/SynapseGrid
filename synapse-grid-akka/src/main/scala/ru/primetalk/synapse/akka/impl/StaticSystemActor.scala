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
package ru.primetalk.synapse.akka.impl

import akka.actor._
import akka.event.{Logging, LoggingAdapter, LoggingReceive}
import org.slf4j.MDC
import ru.primetalk.synapse.akka.SpecialActorContacts.{InitCompleted, _}
import ru.primetalk.synapse.akka._
import ru.primetalk.synapse.core._
import ru.primetalk.synapse.core.components.SignalDist

/** Escalates all exceptions to upper level. This actor is an appropriate default for
  * in-channel actors. */
trait EscalatingActor extends Actor {
  override val supervisorStrategy: SupervisorStrategy =
    defaultSupervisorStrategy
}

/** An actor that contains a Static system and processes signals */
trait AbstractStaticSystemActor extends Actor {

  val systemPath: List[String]
  val system: StaticSystem
  val outputFun: Option[InternalSignalsDist => Any]
  /** None for top level system */
  val parentSystemRef: Option[ActorRef]

  protected val log: LoggingAdapter = Logging(context.system, this)
  var systemState: Map[Contact[_], Any] = system.s0
  val processor: TotalTrellisProducer = createSelfTrellisProducer

  //  def getSubsystemIndex(path:core.SystemPath):Indexed = {
  //    def getSubsystemIndex0(path:core.SystemPath, s:StaticSystem):Indexed = path match {
  //      case head :: Nil =>
  //        val comp = s.getComponentByName(head)
  //        getSubsystemIndex0(tail, comp)
  //      case head :: tail =>
  //        val comp = s.getComponentByName(head)
  //        getSubsystemIndex0(tail, comp)
  //      case Nil =>
  //        s
  //    }
  //    getSubsystemIndex0(path,system)
  //  }
  /** Creates a trellis producer for the system itself. */
  protected
  def createSelfTrellisProducer: TotalTrellisProducer

  val processSignals: List[Signal[_]] => Unit = createSignalProcessor

  protected def createSignalProcessor: List[Signal[_]] => Unit = {
    if (system.inputContacts.contains(SenderInput)) // the check is done at the beginning.
      (ls: List[Signal[_]]) =>
        innerProcessSignals(Signal(SenderInput, sender()) :: ls)
    else
      ls => innerProcessSignals(ls)
  }

  protected def innerProcessSignals(ls: List[Signal[_]]): Unit = {
    MDC.put("akkaSource", "" + self.path)
    val results: List[Signal[_]] = ls.flatMap {
      signal: Signal[_] =>
        val res = processor(systemState, signal)
        systemState = systemState ++ res._1
        res._2
    }
    if (results.nonEmpty) {
      log.info("inner results:" + results)
      val sigs = InternalSignalsDist(systemPath, results.map(system.index.convertSignalToSignalDist))
      outputFun.foreach(_(sigs))
      parentSystemRef.foreach(_ ! sigs)
    }
  }

  def convertInternalSignalsDist(relPath: List[String], signalsDist: List[SignalDist]): List[Signal[_]] = {
    val signals =
      relPath.reverse match {
        case Nil =>
          throw new IllegalArgumentException("The current system should not get signals from itself: " + signalsDist)
        //          case head::Nil =>
        //            system.
        //            signalsDist.map (sd => system.index (sd) )
        case head :: tail =>
          val signals = signalsDist.map(sd =>
            Signal(SubsystemSpecialAnswerContact: Contact[SubsystemDirectSignal0], SubsystemDirectSignalDist(head, sd))
          )
          log.info(s"1: signals = $signals")
          val res = signals.map(signal => tail.foldLeft(signal.asInstanceOf[Signal[Any]])(
            (s: Signal[Any], name: String) =>
              Signal(SubsystemSpecialContact: Contact[SubsystemDirectSignal0],
                SubsystemDirectSignal(name, s)).asInstanceOf[Signal[Any]]
          )
          )
          log.info(s"2: res = $res")
          res
      }
    log.info(s"3: signals =\t$signals")
    signals
  }

  def receive: Receive = LoggingReceive {
    case s@Signal(_, _) =>
      processSignals(s :: Nil)

    /** InternalSignalsDist - message from grand child subsystem.
      * Immediate subsystems are processed by the current system. Others are sent to SubsystemSpecialContact */
    case msg: InternalSignalsDist =>
      val InternalSignalsDist(path, signalsDist) = msg
      log.info(getClass.getSimpleName + " received " + msg)
      val relPath = if (path.startsWith(systemPath)) path.drop(systemPath.size) else throw new IllegalArgumentException("Cannot process path " + path + " at systemPath=" + systemPath)
      if (relPath.isEmpty) {
        throw new IllegalArgumentException("Obtained signals without system name: " + msg + ". sender = " + sender())
        //        log.info(getClass.getSimpleName + " received " + msg)
        //        val innerSignals = signalsDist.map(sd => system.index(sd))
        //        processSignals(innerSignals)
      } else {
        val signalsDist1 = convertInternalSignalsDist(relPath, signalsDist)
        processSignals(signalsDist1)
      }
    case sd: SignalDist =>
      val signal = system.index(sd)
      processSignals(List(signal))
    case nonSignalMessage =>
      log.info("NonSignalMessageReceived: " + nonSignalMessage)
      val s = Signal(NonSignalWithSenderInput, (sender(), nonSignalMessage))
      processSignals(s :: Nil)
  }

  override def preStart(): Unit = {
    if (system.inputContacts.contains(ContextInput))
      processSignals(Signal(ContextInput, context) :: Nil)
    if (system.inputContacts.contains(PreStartInput))
      processSignals(Signal(PreStartInput, context) :: Nil)
    parentSystemRef.foreach(_ ! InitCompleted(self))
  }

  override def postStop(): Unit = {
    if (system.inputContacts.contains(PostStopInput))
      processSignals(Signal(PostStopInput, PostStop) :: Nil)
  }

}

/**
 * Actor that corresponds to the given static system. It will work according to
 * the schema of the system.
 * If there are ActorComponent-s within the system, then they will become children actors of this one.
 *
 * This class is used to create Actors both for top level system and for actor components inside.
 *
 * @param systemPath the list of intermediate systems from parent actor to the system of this actor
 */
class StaticSystemActor(override val systemPath: SystemPath,
                        override val system: StaticSystem,
                        override val outputFun: Option[InternalSignalsDist => Any] = None,
                        override val supervisorStrategy: SupervisorStrategy) extends AbstractStaticSystemActor {
  require(systemPath.nonEmpty, "System path should include at least the system's name")

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
    /*path: */ SystemPath,
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
                               path: SystemPath,
                               system: StaticSystem): TotalTrellisProducer = {
    val actorInnerSubsystemConverter: ComponentDescriptorConverter = {
      case ComponentDescriptor(ActorComponent(subsystem, supervisorStrategy), path1, _) =>
        val actorRef = actorRefFactory.actorOf(Props(
          new StaticSystemActor(path1 :+ subsystem.name, subsystem, None, supervisorStrategy)),
          subsystem.name)
        RuntimeComponentMultiState(subsystem.name, List(), (context: Context, signal) => {
          actorRef.tell(signal, self)
          (context, List())
        })
    }


    val converter = {
      val c =
        SystemConverting.componentToSignalProcessor2(_.toTotalTrellisProducer,
          linkToRuntimeComponent,
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
                  defaultSupervisorStrategy)(path: SystemPath,
                                             system: StaticSystem,
                                             outputFun: Option[InternalSignalsDist => Any] = None): ActorRef =
    actorRefFactory.actorOf(Props(
      new StaticSystemActor(path :+ system.name, system, outputFun, supervisorStrategy)),
      system.name)

}
