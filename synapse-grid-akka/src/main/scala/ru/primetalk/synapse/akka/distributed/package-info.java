/**
 * Distributed system of actors.
 *
 * The system is converted to a set of RouterBecome actors at the same level.
 * Directly under the HostActor.
 * Then at a particular host some of actual system actor are also created (ActorForSystemOneLevel).
 * When created they register themselves at the corresponding router.
 *
 * The hierarchy of subsystems becomes "flat" collection of systems under HostActor.
 * And these subsystems do not interact with each other directly instead they send messages via
 * routers.
 * @author zhizhelev, 26.03.15.
 */
package ru.primetalk.synapse.akka.distributed;