///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2014                                   //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2014.
 * All rights reserved.
 * Authors: A.Zhizhelev
 *
 * Created: 07.06.14, zhizhelev
 */
package ru.primetalk.synapse.akka.distributed

import ru.primetalk.synapse.core
import akka.actor.{SupervisorStrategy, Props, ActorRefFactory, ActorPath}
import ru.primetalk.synapse.akka._
import ru.primetalk.synapse.akka.ActorComponent
import ru.primetalk.synapse.core.components.{Component, ComponentWithInternalStructure}

/**
Deployment of a system over a cluster of nodes
 ----------------------------------------------
 The realm knows about other hosts and
 the deployment of systems over all the hosts.
  */
case class RealmDescriptor(topLevelSystem: ComponentWithInternalStructure, deployment: Vector[(List[core.SystemPath], ActorPath)]) {
  /** Looks up for the systemPath in the deployment descriptor
    * and constructs an appropriate ActorPath for it. */
  def getRouterPath(systemPath: core.SystemPath): ActorPath = {
    val hosts = deployment.filter(_._1.contains(systemPath)).map(_._2)
    if (hosts.size != 1)
      throw new IllegalStateException(s"For path=$systemPath found != 1 hosts.")
    hosts.head / systemPathToActorName(systemPath)
  }

  /** Looks up for the systemPath in the deployment descriptor
    * and constructs an appropriate ActorPath for it. */
  def getSystemImplPath(systemPath: core.SystemPath): ActorPath = {
    val hosts = deployment.filter(_._1.contains(systemPath)).map(_._2)
    if (hosts.size != 1)
      throw new IllegalStateException(s"For path=$systemPath found != 1 hosts.")
    hosts.head / ("subsystem_" + systemPathToActorName(systemPath))
  }

  /** List paths of systems that should be created at this host. */
  def pathsForHost(host: ActorPath) =
    deployment.filter(_._2 == host).flatMap(_._1).toSet

  private
  def systemPathToActorName(path: core.SystemPath) =
    path.mkString("_")

  /**
   * Creates all routers for the system.
   * @param s the system to create routers
   * @return collection of SystemPath -> ActorRef - routers for every InnerActorSubsystem.
   */
  def createRouters(s: Component)(context: ActorRefFactory) =
    actorInnerSubsystems2(s).map(_._1).map {
      path =>
        val actorName = systemPathToActorName(path)
        //        log.info("router " + actorName + " for " + path)
        (path, context.actorOf(Props[RouterBecome], actorName))
    }

  /** Recursively finds all subsystems of the system. */
  def actorInnerSubsystems(component: Component): List[(core.SystemPathReversed, ActorComponent)] =
    core.subcomponents(component).collect { case (pathRev, actorInnerSubsystem: ActorComponent) => (pathRev, actorInnerSubsystem)}

  def actorInnerSubsystems2(component: Component): List[(core.SystemPath, ActorComponent)] =
    actorInnerSubsystems(component).map(p => (p._1.reverse, p._2))


  /** Creates subsystem actors for the given top level StaticSystem and
    * the set of paths that belongs to this host.
    *
    * The subsystems send signals to other actors via router actors that should have been
    * created. */
  def createSubsystems(s: Component,
                       pathsForThisHost: Set[core.SystemPath])(context: ActorRefFactory,
                                                               supervisorStrategy: SupervisorStrategy =
                                                               defaultSupervisorStrategy,
                                                               outputFun: Option[InternalSignalsDist => Any] = None, realm: RealmDescriptor) = {
    for {
      (path, actorSubsystem) <- actorInnerSubsystems2(s)
      if pathsForThisHost.contains(path) // we construct only those subsystems that should be instantiated at this host
      subsystem = actorSubsystem.subsystem
      actorName = "subsystem_" + systemPathToActorName(path)
    } yield
      context.actorOf(Props(new ActorForSystemOneLevel(path, subsystem, supervisorStrategy, realm)), actorName)
  }

}

/**
 * Identifier of a host.
 * "akka.tcp://actorSystemName@10.0.0.1:2552/user/actorName"
 * The host is an actor that is responsible for creation and recreation of the system in the particular host.
 */
final case class HostId(host: String, port: Int, actorSystemName: String, hostActorName: String = "synapse") {
  override
  def toString = s"akka.tcp://$actorSystemName@$host:$port/user/$hostActorName"

  def toActorPath =
    ActorPath.fromString(toString)
}

