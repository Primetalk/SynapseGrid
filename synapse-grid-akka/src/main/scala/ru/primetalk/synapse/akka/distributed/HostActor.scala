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
 * Created: 08.06.14, zhizhelev
 */
package ru.primetalk.synapse.akka.distributed

import akka.actor.{Actor, SupervisorStrategy}
import ru.primetalk.synapse.core.syntax._
import ru.primetalk.synapse.core.syntax.given
import akka.event.Logging
import ru.primetalk.synapse.akka.InternalSignalsDist

/**
 * Deploys a single part of a distributed system that can run over a few hosts.
 *
 * The deployment incarnation at a single host. Creates routers and corresponding
 * actors for systems.
 *
 * @param hostId identifier of the current host
 * @param supervisorStrategy supervisor strategy
 */
class HostActor(hostId: HostId,
                realm: RealmDescriptor,
                supervisorStrategy: SupervisorStrategy,
                outputFun: Option[List[Signal0] => Any] = None) extends Actor {
  val log = Logging(context.system, this)

  val pathsForThisHost = realm.pathsForHost(hostId.toActorPath)

  val routers = realm.createRouters(realm.topLevelSystem)(context).toMap

  val actorSubsystems = realm.createSubsystems(realm.topLevelSystem, pathsForThisHost)(context, supervisorStrategy, None, realm) //realm.topLevelSystem, pathsForThisHost)(context, realm)

  def receive = {
    case InternalSignalsDist(_, list) =>
      val idx: ContactsIndex = realm.topLevelSystem.toStaticSystem.index
      val sss: List[Signal0] = list.map(idx.apply(_))
      outputFun.foreach(f => f(sss))
    case msg =>
      log.info("HostActor: " + msg)
  }

}

