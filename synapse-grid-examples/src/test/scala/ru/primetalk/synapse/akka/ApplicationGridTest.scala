package ru.primetalk.synapse.akka

import akka.actor._
import akka.actor.ActorDSL._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import ru.primetalk.synapse.akka._
import org.scalatest.FunSuite
import ru.primetalk.synapse.core.Signal
import scala.concurrent.duration._

/**
 * @author s.kazantsev, a.zhizhelev
 */
@RunWith(classOf[JUnitRunner])
class ApplicationGridTest extends FunSuite{
  test("test"){
    implicit val actorSystem = ActorSystem()
    implicit val i = inbox()
    val applicationGrid: ApplicationGrid = new ApplicationGrid
    val actor = applicationGrid.toStaticSystem.toActorTree(actorSystem)
    actor ! Signal(applicationGrid.start, ())
//    Thread.sleep(2000)
    i.receive(10.second) match {
        case data =>
          println("ok: "+data)
      }
    actorSystem.shutdown()
  }
}
