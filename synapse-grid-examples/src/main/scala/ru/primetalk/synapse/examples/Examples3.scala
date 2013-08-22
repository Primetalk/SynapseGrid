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
 * Created: 17.07.13, zhizhelev
 */
package ru.primetalk.synapse.examples

import ru.primetalk.synapse.core._
import ru.primetalk.synapse.akka.{ActorContainerBuilder, ActorSystemBuilder}
import scala.language.implicitConversions
import akka.actor.ActorSystem

object Examples3 {
  object Control extends Contact[Any]
  object Monitor extends Contact[Int]

  object Input extends Contact[Any]
  object Output extends Contact[Any]

  class PingPongBuilder(name:String) extends ActorSystemBuilder {
    setSystemName(name)
    inputs(Input)
    outputs(Output)
//    Input.foreach(a=>println(name+":"+a))
    Input.ifConst("ping").const("pong")>>Output
    Input.ifConst("pong").const("ping")>>Output
  }
  class WiringBuilder(name:String, input:Contact[Any],output:Contact[Any]) extends ActorContainerBuilder {
    setSystemName(name)
    inputs(input)
    outputs(output)
    input >> Input
    Output >> output
    addActorSubsystem(new PingPongBuilder(name+"InnerActor"))
  }
  object A extends Contact[Any]
  object B extends Contact[Any]

  class SuperSystemBuilder(actorSystem:ActorSystem = null) extends SystemBuilder {
    addSubsystem(new WiringBuilder("AB", A,B))
    addSubsystem(new WiringBuilder("BA", B,A))

    inputs(Control)
    outputs(Monitor)
    Control.ifConst("start").const("ping") >> A

    val counter = state[Int]("counter", 0)
//    A.foreach(println)
    A.getState(counter).map(_+1, "_+1").saveTo(counter)
    val stop = Control.ifConst("stop")
    stop.getState(counter) >> Monitor//.foreach(cnt => println("The number of iterations = "+cnt))
//    stop.exec{actorSystem.shutdown()}
  }
}
