///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev
 *
 * Created: 13.02.14, zhizhelev
 */
package ru.primetalk.synapse.akka.distributed

import akka.actor._
import ru.primetalk.synapse.core.StaticSystem
import ru.primetalk.synapse.core
import ru.primetalk.synapse.akka.InternalSignals
import ru.primetalk.synapse.akka.ActorInnerSubsystem
import scala.Some

/**
 * Deploys a single part of a distributed system that can run over a few hosts.
 */
object DistributedActors {

  /** Where the root of each host is placed.
    */
  type HostLayout = HostId => ActorPath

  /** Collection of relations between hostId and system paths. */
  type DeploymentDescriptor = Vector[(HostId, List[core.SystemPath])]

  //  case class SubsystemPath(hostId:HostId, path:)

  /*
   Deployment of systems over a cluster of nodes.
   ----------------------------------------------

   */

  /**
   * Describes system's remote location.
   *
   * @param systemPath - the subsystem path
   * @param hostname - address of an actor system that
   * @param port tcp port where ActorSystem resides.
   */
  case class DeploymentDescriptor0(systemPath: core.SystemPath, hostname: String, port: Int)


  type ActorMap = Map[core.SystemPath, ActorRef]

  //  def getSubsystemActorPath(layout:HostLayout)(subsystemPath:core.SystemPath):ActorPath = {
  //    val hostPath = layout(subsystemPath.hostId)
  //    hostPath/subsystemPath.path
  //  }
  val as = ActorSystem().actorSelection("some")
  val ap = ActorPath.fromString("")
  ap.address

  //  val ar = ActorSystem().ac

  /** Recursively finds all subsystems of the system. */
  def actorInnerSubsystems(component: core.Component): List[(core.SystemPathReversed, core.Component)] =
    core.components(component).filter(p => p._2.isInstanceOf[ActorInnerSubsystem])

  def actorInnerSubsystems2(component: core.Component): List[(core.SystemPath, core.Component)] =
    actorInnerSubsystems(component).map(p => (p._1.reverse, p._2))

  def systemPathToActorName(path: core.SystemPath) =
    path.mkString("_")

  /**
   * Creates all routers for the system.
   * @param s the system to create routers
   * @return collection of SystemPath -> ActorRef - routers for every InnerActorSubsystem.
   */
  def createRouters(s: StaticSystem)(context: ActorRefFactory) =
    actorInnerSubsystems2(s).map(p =>
      (p._1, context.actorOf(Props[RouterBecome], systemPathToActorName(p._1))))


  def pathsForThisHost(deployment: DeploymentDescriptor, thisHost: HostId) = deployment.toMap.getOrElse(thisHost, Nil).toSet

  def createSubsystems(s: StaticSystem, pathsForThisHost: Set[core.SystemPath])(context: ActorRefFactory) {
    val actorComponents = actorInnerSubsystems2(s).filter(p => pathsForThisHost.contains(p._1))
    //    context.actorOf(Props())
  }

  /**
   *
   * @param actorRefFactory the factory to create actors at
   * @param hostId identifier of the current host
   * @param supervisorStrategy supervisor strategy
   */
  class Actors(actorRefFactory: ActorRefFactory,
               hostId: HostId,
               system: StaticSystem,
               supervisorStrategy: SupervisorStrategy
               //              =
               //              defaultSupervisorStrategy
               ,
               outputFun: Option[InternalSignals => Any] =
               None) {

    val routers = createRouters(system)(actorRefFactory).toMap
    //    : ActorRef =
    //  /** Converts top level system to top level actor. */
    //  def toActorTree(actorRefFactory: ActorRefFactory,
    //                  supervisorStrategy:SupervisorStrategy =
    //                  defaultSupervisorStrategy)(path:List[String], system: StaticSystem, outputFun:Option[InternalSignals => Any] = None): ActorRef =
    //    actorRefFactory.actorOf(Props(
    //      new StaticSystemActor(path,system, outputFun, supervisorStrategy)),
    //      system.name)
  }

}

/**
 * Identifier of a host.
 */
final case class HostId(host: String, port: Int)

/** Routes everything except register/unregister commands to
  * the system that has registered with the router.
  */
class Router extends Actor {
  var destination: Option[ActorRef] = None

  def receive = {
    case Register(actor) =>
      destination = Some(actor)
    case Unregister =>
      destination = None
    case m =>
      destination.foreach(_ forward m)
  }
}

sealed trait RouterCommand

/**
 * Registers the actor as a destination of all Router's input.
 * @param actor the destination of subsequent messages.
 */
case class Register(actor: ActorRef) extends RouterCommand

/**
 * Stops registration. All messages will be sent to DeathWatch.
 */
case object Unregister extends RouterCommand

/** Routes everything except register/unregister commands to
  * the system that has registered with the router.
  */
class RouterBecome extends Actor {
  def receive = {
    case Register(actor) =>
      context.become({
        case m => actor forward m
      })
  }
}
