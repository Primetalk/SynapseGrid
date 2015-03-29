///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 *
 * Created: 01.08.13, zhizhelev
 */
package ru.primetalk.synapse.examples

import ru.primetalk.synapse.akka._
import ru.primetalk.synapse.examples.Examples3.{Control, SuperSystemBuilder}
import akka.actor.ActorSystem
import akka.actor.ActorDSL._
import ru.primetalk.synapse.core.Signal
import scala.concurrent.duration._

object Example3App extends App  {
  implicit val actorSystem = ActorSystem()
  val actor = new SuperSystemBuilder(actorSystem).toStaticSystem.toTopLevelActor(actorSystem)
  implicit val i = inbox()
  actor ! Signal(Control,"start")
  Thread.sleep(20000)

//  val mb = akka.actor.Actor actorSystem.
  actor ! Signal(Control,"stop")
  i.receive(1.second) match {
    case Signal(_, data) =>
      println("out="+data)
  }
  actorSystem.shutdown()
}
