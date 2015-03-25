package ru.primetalk.synapse.akka.impl

import akka.actor.{ActorRef, ActorRefFactory}
import ru.primetalk.synapse.akka.{StaticSystemActor, InternalSignalsDist}
import ru.primetalk.synapse.core.StaticSystem

/**
 * @author zhizhelev, 25.03.15.
 */
trait StaticSystemAkkaApi {
  implicit class RichStaticSystemSystem(s: StaticSystem) {
    /**
     * The main DSL function to convert a StaticSystem to an actor tree.
     * Returns top level actor.
     * @param threadSafeOutputFun - a function that will receive output signals of the actor.
     *                            Should be thread safe!!!
     *                            May be omitted. In the latter case the output signals
     *                            are ignored.*/
    def toActorTree(threadSafeOutputFun: Option[InternalSignalsDist => Any] = None)
                   (implicit actorRefFactory: ActorRefFactory): ActorRef =
      StaticSystemActor.toActorTree(actorRefFactory)(List(), s, threadSafeOutputFun)

    def toActorTree(implicit actorRefFactory: ActorRefFactory): ActorRef =
      toActorTree(None)(actorRefFactory)
  }


}
