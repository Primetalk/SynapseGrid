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
import ru.primetalk.synapse.akka._
import ru.primetalk.synapse.akka.impl.AbstractStaticSystemActor
//import ru.primetalk.synapse.core.components.StaticSystem
import ru.primetalk.synapse.core.syntax._
import ru.primetalk.synapse.core.syntax.given
import scala.concurrent.Await
import scala.concurrent.duration._

/** An actor for a subsystem. Whenever a message is going
  * to be sent to an inner actor or to the parent actor, it is
  * sent to the router instead. This allows to redirect the
  * message to another host for instance. */
class ActorForSystemOneLevel(override val systemPath: SystemPath,
                             override val system: StaticSystem,
                             override val supervisorStrategy: SupervisorStrategy,
                             val realm: RealmDescriptor) extends AbstractStaticSystemActor {

  require(systemPath.nonEmpty, "System path should include at least the system's name")
  override
  val outputFun: Option[InternalSignalsDist => Any] = None

  log.info("ActorForSystemOneLevel: " + self.path)
  lazy val parentSystemRef: Option[ActorRef] =
    if systemPath.isEmpty then // for the root system.
      None
    else {
      implicit val timeout: Timeout = Timeout(5.seconds)
      val name = realm.getRouterPath(systemPath.take(systemPath.size - 1))
      val actorSelection = context.actorSelection(name)
      log.info(s"Name=$name, sel=$actorSelection")
      Some(Await.result(
        actorSelection.resolveOne(),
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
    toSingleSignalProcessorOneLevel(context, self, realm)(systemPath, system)
  }

  override
  def preStart(): Unit = {
    selfRouter ! Register(self)
  }

  /** Creates a total trellis producer that only processes signals until another inner
    * actor system. For that system signals are sent to an actor by it's name.
    * Actor name is done during configuration time.
    */
  def toSingleSignalProcessorOneLevel(actorRefFactory: ActorRefFactory,
                                      self: ActorRef = Actor.noSender,
                                      realm: RealmDescriptor
                                       )(
                                       path: List[String],
                                       system: StaticSystem): TotalTrellisProducer = {
    val actorInnerSubsystemConverter: ComponentDescriptorConverter = {
      case ComponentDescriptor(ActorComponent(subsystem, _), path1, _) =>
        // TODO: obtain ActorRef for better performance
        val childRouterPath = realm.getRouterPath(path1 ++ List(subsystem.name))
        val childRouterSelection = actorRefFactory.actorSelection(childRouterPath)

        RuntimeComponentMultiState(subsystem.name, List(), (context: Context, signal: Signal0) => {
          val sd = subsystem.index(signal)
          //          log.info("To inner actor system: {Signal=" + signal + ", SignalDist =" + sd + "} to " + childRouterSelection)
          childRouterSelection.tell(sd, self)
          (context, List())
        })
    }

    val converter =
      SystemConverting.
        componentToSignalProcessor2(
          _.toTotalTrellisProducer,
          linkToRuntimeComponent,
          actorInnerSubsystemConverter)

    SystemConverting.
      systemToRuntimeSystem(path, system, converter, system.outputContacts).
      toTotalTrellisProducer
  }

}
