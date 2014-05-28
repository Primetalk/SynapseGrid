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
import akka.util.Timeout
import ru.primetalk.synapse.core._
import ru.primetalk.synapse.core
import ru.primetalk.synapse.akka._
import ru.primetalk.synapse.core.SystemConvertingSupport._
import ru.primetalk.synapse.core.RuntimeComponentMultiState
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Deploys a single part of a distributed system that can run over a few hosts.
 */
object DistributedActors {

  /** Where the root of each host is placed.
    */
  type HostLayout = HostId => ActorPath

  /** Collection of relations between hostId and system paths. */
  type DeploymentDescriptor = Vector[(HostId, List[core.SystemPath])]

  def systemPathToActorName(path: core.SystemPath) =
    path.mkString("_")

  /** Converts system path to actor path for actorSelection. */
  //  type SystemPathToActorPathResolver = (core.SystemPath) => ActorPath

  /** The realm knows about other hosts and
    * the deployment of systems over all the hosts. */
  case class Realm(topLevelSystem: ComponentWithInternalStructure, deployment: DeploymentDescriptor) {
    /** Looks up for the systemPath in the deployment descriptor
      * and constructs an appropriate ActorPath for it. */
    def getRouterPath(systemPath: core.SystemPath): ActorPath = {
      val hosts = deployment.filter(_._2.contains(systemPath)).map(_._1)
      if (hosts.size != 1)
        throw new IllegalStateException(s"For path=$systemPath found != 1 hosts.")
      val host = hosts(0)
      host.toActorPath / systemPathToActorName(systemPath)
    }

    /** Looks up for the systemPath in the deployment descriptor
      * and constructs an appropriate ActorPath for it. */
    def getSystemImplPath(systemPath: core.SystemPath): ActorPath = {
      val hosts = deployment.filter(_._2.contains(systemPath)).map(_._1)
      if (hosts.size != 1)
        throw new IllegalStateException(s"For path=$systemPath found != 1 hosts.")
      val host = hosts(0)
      host.toActorPath / ("subsystem_" + systemPathToActorName(systemPath))
    }

    /** List paths of systems that should be created at this host. */
    def pathsForHost(host: HostId) =
      deployment.toMap.getOrElse(host, Nil).toSet

  }

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

  /** Recursively finds all subsystems of the system. */
  def actorInnerSubsystems(component: core.Component): List[(core.SystemPathReversed, ActorInnerSubsystem)] =
    core.components(component).collect { case (pathRev, actorInnerSubsystem: ActorInnerSubsystem) => (pathRev, actorInnerSubsystem)}

  def actorInnerSubsystems2(component: core.Component): List[(core.SystemPath, ActorInnerSubsystem)] =
    actorInnerSubsystems(component).map(p => (p._1.reverse, p._2))

  /**
   * Creates all routers for the system.
   * @param s the system to create routers
   * @return collection of SystemPath -> ActorRef - routers for every InnerActorSubsystem.
   */
  def createRouters(s: core.Component)(context: ActorRefFactory) =
    actorInnerSubsystems2(s).map(_._1).map {
      path =>
        val actorName = systemPathToActorName(path)
        println("router " + actorName + " for " + path)
        (path, context.actorOf(Props[RouterBecome], actorName))
    }


  /** Creates subsystem actors for the given top level StaticSystem and
    * the set of paths that belongs to this host.
    *
    * The subsystems send signals to other actors via router actors that should have been
    * created. */
  def createSubsystems(s: core.Component, pathsForThisHost: Set[core.SystemPath])(context: ActorRefFactory,
                                                                                  supervisorStrategy: SupervisorStrategy =
                                                                                  defaultSupervisorStrategy,
                                                                                  outputFun: Option[InternalSignals => Any] = None, realm: Realm) = {
    for {
      (path, actorSubsystem) <- actorInnerSubsystems2(s)
      if pathsForThisHost.contains(path) // we construct only those subsystems that should be instantiated at this host
      subsystem = actorSubsystem.subsystem
      actorName = "subsystem_" + systemPathToActorName(path)
    } yield
      context.actorOf(Props(new ActorForSystemOneLevel(path, subsystem, outputFun, supervisorStrategy, realm)), actorName)
  }

  /**
   * The deployment incarnation at a single host. Creates routers and corresponding
   * actors for systems.
   *
   *
   *
   * @param hostId identifier of the current host
   * @param supervisorStrategy supervisor strategy
   */
  class HostActor(hostId: HostId,
                  realm: Realm,
                  supervisorStrategy: SupervisorStrategy,
                  outputFun: Option[InternalSignals => Any] = None) extends Actor {

    val pathsForThisHost = realm.pathsForHost(hostId)

    val routers = createRouters(realm.topLevelSystem)(context).toMap

    val actorSubsystems = createSubsystems(realm.topLevelSystem, pathsForThisHost)(context, supervisorStrategy, outputFun, realm) //realm.topLevelSystem, pathsForThisHost)(context, realm)

    def receive = {
      case msg =>
        println("HostActor: " + msg)
    }

    println("HostActor.routers = " + routers)
    println("HostActor.actorSubsystems = " + actorSubsystems)

    //    def connect
  }


  /** Creates a total trellis producer that only processes signals until another inner
    * actor system. For that system signals are sent to an actor by it's name.
    * Actor name is done during configuration time.
    */
  def toSingleSignalProcessorOneLevel(actorRefFactory: ActorRefFactory,
                                      self: ActorRef = Actor.noSender,
                                      realm: Realm
                                       )(
                                       path: List[String],
                                       system: StaticSystem): TotalTrellisProducer = {
    val actorInnerSubsystemConverter: ComponentDescriptorConverter = {
      case ComponentDescriptor(ActorInnerSubsystem(subsystem, supervisorStrategy), path1, _) =>

        // TODO: obtain ActorRef for better performance
        val childRouterPath = realm.getRouterPath(path1 ++ List(subsystem.name))
        val childRouterSelection = actorRefFactory.actorSelection(childRouterPath)
        println(s"actorInnerSubsystemConverter childRouterPath=$childRouterPath; childRouterSelection=$childRouterSelection")
        childRouterSelection ! "hello"

        RuntimeComponentMultiState(subsystem.name, List(), (context: Context, signal) => {
          println("rcms: " + signal)
          childRouterSelection.tell(signal, self)
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
  println("RouterBecome[" + self.path + "] created")
  def receive = {
    case Register(actor) =>

      println("RouterBecome: " + self.path + " received " + Register(actor))
      context.become {
        case m =>
          println("RouterBecome[" + self.path + "](" + actor + "): received " + m)
          actor forward m
      }
  }
}

/** An actor for a subsystem. Whenever a message is going
  * to be sent to an inner actor or to the parent actor, it is
  * sent to the router instead. This allows to redirect the
  * message to another host for instanse. */
class ActorForSystemOneLevel(override val systemPath: core.SystemPath,
                             override val system: StaticSystem,
                             override val outputFun: Option[InternalSignals => Any] = None,
                             override val supervisorStrategy: SupervisorStrategy,
                             val realm: DistributedActors.Realm) extends AbstractStaticSystemActor {
  println("ActorForSystemOneLevel: " + self.path)
  lazy val parentSystemRef: Option[ActorRef] =
    if (systemPath.isEmpty) // for the root system.
      None
    else {
      implicit val timeout = Timeout(5 seconds)
      Some(Await.result(
        context.actorSelection(DistributedActors.systemPathToActorName(systemPath.take(systemPath.size - 1))).resolveOne(),
        atMost = timeout.duration
      ))
    }

  private
  val selfRouter = {
    context.actorSelection(realm.getRouterPath(systemPath))
  }

  /** Creates a trellis producer for the system itself. */
  protected
  def createSelfTrellisProducer: TotalTrellisProducer = {
    DistributedActors.toSingleSignalProcessorOneLevel(context, self, realm)(systemPath, system)
  }

  override
  def preStart() {
    selfRouter ! Register(self)
  }
}
