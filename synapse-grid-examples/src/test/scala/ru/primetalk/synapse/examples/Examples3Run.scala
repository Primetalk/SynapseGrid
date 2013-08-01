///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////

/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 * <p/>
 * Created: 17.07.13, zhizhelev
 */
package ru.primetalk.synapse.examples

import org.scalatest.FunSuite

import ru.primetalk.synapse.akka._
import ru.primetalk.synapse.examples.Examples3.{Control, SuperSystemBuilder}
import akka.actor.ActorSystem
import ru.primetalk.synapse.core.Signal

class Examples3Run extends FunSuite {
  test("Run example 3"){
    val actorSystem = ActorSystem()
    val actor = new SuperSystemBuilder(actorSystem).toStaticSystem.toActorTree(actorSystem)
    actor ! Signal(Control,"start")
    Thread.sleep(1000)
    actor ! Signal(Control,"stop")
  }

}
