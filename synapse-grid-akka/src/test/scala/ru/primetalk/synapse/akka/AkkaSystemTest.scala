package ru.primetalk.synapse.akka

import java.util.concurrent.{CountDownLatch, TimeUnit}
import akka.actor.{ActorDSL, ActorSystem}
import akka.actor.ActorDSL._
import ru.primetalk.synapse.core.syntax._
import ru.primetalk.synapse.core.syntax.given
import ru.primetalk.synapse.slf4j._
import akka.actor.actorRef2Scala

import scala.concurrent.Await
import scala.concurrent.duration.Duration

import org.junit.Test
/**
 * @author zhizhelev, 24.08.15.
 */
class AkkaSystemTest {

  case class Ping(isPing:Boolean = true){
    def invert: Ping = Ping(!isPing)
  }
  class PingPongSubsystem(name:String) extends BaseTypedSystem(name) {
    val ping: Contact[Ping] = input[Ping]("ping")
    val pong: Contact[Ping] = output[Ping]("pong")
    override protected def defineSystem(implicit sb: SystemBuilder): Unit = {
      LinkBuilderOps(ping -> pong).map(_.invert, "invert")
    }
  }
  class DecSubsystemInterface(b: OuterInterfaceBuilder){
    val in: Contact[Int] = b.input[Int]("in")
    val out: Contact[Int] = b.output[Int]("out")
  }
  def decSubsystemImplementation(name: String): EncapsulationBuilder[DecSubsystemInterface] = new EncapsulationBuilder(name)(new DecSubsystemInterface(_)){
    val akkaExt: ActorSystemBuilderExtension = sb.extend[ActorSystemBuilderExtension](ActorSystemBuilderExtensionId)
    LinkBuilderOps(outer.in -> outer.out).map(_ -1, "_ - 1")
    outer.in.getState(akkaExt.self)//.info(s"$name.self="+_)
  }
//  /** Subsystem that decrements the counter*/
//  class DecSubsystem(name:String) extends BaseTypedSystem(name) {
//    val in = input[Int](name+"in")
//    val out = output[Int](name+"out")
//    override protected def defineSystem(implicit sb: SystemBuilder): Unit = {
//      val akkaExt = sb.extend[ActorSystemBuilderExtension](ActorSystemBuilderExtensionId)
//      (in -> out).map(_ -1, "--_")
//      in.getState(akkaExt.self).info(s"$name.self="+_)
//    }
//  }
  /** The system contains two decrementing subsystems
    * running in separate actors.
    *
    */
  class CounterSystem extends BaseTypedSystem {
    val counter: Contact[Int] = input[Int]("counter")
    val zero: Contact[Int] = output[Int]("zero")
    override protected def defineSystem(implicit sb: SystemBuilder): Unit = {
      val akkaExt = sb.extend[ActorSystemBuilderExtension](ActorSystemBuilderExtensionId)
      akkaExt.self
//      val ping = new PingPongSubsystem("Ping")
      val dec1 = //defineEncapsulation("dec1")(DecOuterImplementation) //new DecSubsystem("dec1")
      encapsulateAsActor(decSubsystemImplementation("dec1"))
//      sb.addComponent(dec1.toActorComponent())
      val dec2 = //defineEncapsulation("dec2")(DecOuterImplementation) // new DecSubsystem("dec2")
      encapsulateAsActor(decSubsystemImplementation("dec2"))
//      sb.addSubsystem(dec2.toActorComponent().encapsulate("d2"))
      counter.filter(_>0, ">0") >> dec1.in
      dec1.out >> dec2.in
      dec2.out.filter(_ >0,">0") >> dec1.in
      dec2.out.filter( _ == 0,"==0") >> zero
    }
  }

  @Test def `decrementing subsystems`(): Unit = {
    val s = new CounterSystem
    s.toDot(2).saveTo("decrementing.dot")
    implicit val as: ActorSystem = ActorSystem()
    try {
      implicit val in: ActorDSL.Inbox = inbox()
      val latch = new CountDownLatch(1)
      val t = s.toActorTree(Some((i:InternalSignalsDist)=>  latch.countDown()))
      t ! Signal(s.counter, 10)
//      val zero = in.receive()
      assert(latch.await(5, TimeUnit.SECONDS))
    } finally
      Await.result(as.terminate(), Duration.Inf)
  }
}
