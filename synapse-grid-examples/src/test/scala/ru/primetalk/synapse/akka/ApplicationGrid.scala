package ru.primetalk.synapse.akka

import akka.actor.ActorRef
import ru.primetalk.synapse.core._
//import ru.primetalk.synapse.akka._ // do not import because the tests are in the same package.

trait ProcessGrid extends BaseTypedSystem{
  def prefix:String// = "system"
  val start = input[Unit](prefix+".start")

  val initialized = output[Unit](prefix+".initialized")
  
}
/**
 * @author s.kazantsev, a.zhizhelev
 */
class ApplicationGrid extends ProcessGrid {

  def prefix = "app"
  override 
  protected def defineSystem(implicit sb:SystemBuilder): Unit = {
    val akkaExt = //sb:ru.primetalk.synapse.akka.AkkaSystemBuilderExtension//..]//
    sb.extend(akkaExtensionId)

    val myEtlGrid = new MyEtlGrid
    sb.addActorSubsystem(myEtlGrid)

//    SpecialActorContacts.ContextInput.map(ctx => Some(ctx.sender())).saveTo(sender)

    start >> myEtlGrid.start

//    sb.inputs(ContextInput)
//    ContextInput.foreach(ctx => println("Context: "+ctx))
    val startSender = sb.state[ActorRef]("startSender", akka.actor.Actor.noSender)
    start.getState(akkaExt.sender).saveTo(startSender)

    myEtlGrid.initialized.foreach(u=>println("AppGrid.myEtlGrid.initialized"))
    myEtlGrid.initialized.getState(startSender).foreach{
      case senderVal =>
        senderVal ! ()
    }
  }
  
  class MyEtlGrid extends ProcessGrid {
    def prefix = "my"
    override
    protected def defineSystem(implicit sb:SystemBuilder): Unit = {      
      start.foreach(u => println( "MyEtlGrid started" ))
      start >> initialized
    }
    
  }
}
