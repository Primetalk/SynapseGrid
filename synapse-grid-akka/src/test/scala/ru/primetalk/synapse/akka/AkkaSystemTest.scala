package ru.primetalk.synapse.akka

import java.util.concurrent.{CountDownLatch, TimeUnit}

import akka.actor.ActorSystem
import akka.actor.ActorDSL._
import org.scalatest.FunSuite
import ru.primetalk.synapse.core._
import ru.primetalk.synapse.slf4j._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * @author zhizhelev, 24.08.15.
 */
class AkkaSystemTest extends FunSuite {

  case class Ping(isPing:Boolean = true){
    def invert = Ping(!isPing)
  }
  class PingPongSubsystem(name:String) extends BaseTypedSystem(name) {
    val ping = input[Ping]("ping")
    val pong = output[Ping]("pong")
    override protected def defineSystem(implicit sb: SystemBuilder): Unit = {
      (ping -> pong).map(_.invert, "invert")
    }
  }
  class DecSubsystemInterface(b:OuterInterfaceBuilder){
    val in = b.input[Int]("in")
    val out = b.output[Int]("out")
  }
  def decSubsystemImplementation(name:String): EncapsulationBuilder[DecSubsystemInterface] = new EncapsulationBuilder(name)(new DecSubsystemInterface(_)){
    val akkaExt = sb.extend[ActorSystemBuilderExtension](ActorSystemBuilderExtensionId)
    (outer.in -> outer.out).map(_ -1, "_ - 1")
    outer.in.getState(akkaExt.self).info(s"$name.self="+_)
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
    val counter = input[Int]("counter")
    val zero = output[Int]("zero")
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

  test("decrementing subsystems"){
    val s = new CounterSystem
    s.toDot(2).saveTo("decrementing.dot")
    implicit val as = ActorSystem()
    try {
      implicit val in = inbox()
      val latch = new CountDownLatch(1)
      val t = s.toActorTree(Some((i:InternalSignalsDist)=>  latch.countDown()))
      t ! Signal(s.counter, 10)
//      val zero = in.receive()
      assert(latch.await(5, TimeUnit.SECONDS))
    } finally
      Await.result(as.terminate(), Duration.Inf)
  }
}
