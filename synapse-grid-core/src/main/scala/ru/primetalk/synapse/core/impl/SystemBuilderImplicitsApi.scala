package ru.primetalk.synapse.core.impl

import ru.primetalk.synapse.core
import ru.primetalk.synapse.core._

import scala.util.Try
import scala.language.implicitConversions

/**
 * @author zhizhelev, 25.03.15.
 */
trait SystemBuilderImplicitsApi {

  def extendBasicSystemBuilder[T <: SystemBuilderExtension]( implicit sb: BasicSystemBuilder,
        extensionInstance: SystemBuilderExtensionId[T]): T =
    sb.extend(extensionInstance)

  implicit def basicSystemBuilderToAdvanced(implicit sb: BasicSystemBuilder): SystemBuilderAdv =
    new SystemBuilderAdvC(sb)

  implicit def implLinkBuilder[T1, T2](c: (Contact[T1], Contact[T2]))(implicit sb: BasicSystemBuilder): LinkBuilderOps[T1, T2] =
    new core.LinkBuilderOps(c)(sb)

  implicit def implTryLinkBuilder[T1, T2](p: (Contact[T1], Contact[Try[T2]]))(implicit sb: BasicSystemBuilder): TryLinkBuilderOps[T1, T2] =
    new core.TryLinkBuilderOps(p)(sb)

  implicit def implDirectLinkBuilder[T1, T2 >: T1](p: (Contact[T1], Contact[T2]))(implicit sb: BasicSystemBuilder): DirectLinkBuilderOps[T1, T2] =
    new core.DirectLinkBuilderOps(p)(sb)

  implicit def implRichContactPair[S, T](c: Contact[(S, T)])(implicit sb: BasicSystemBuilder): ContactPairOps[S, T] =
    new core.ContactPairOps(c)(sb)

  implicit def zippingLink[S, T](c: (Contact[T], Contact[(S, T)]))(implicit sb: BasicSystemBuilder): ZippingLinkOps[S, T] =
    new core.ZippingLinkOps[S, T](c: (Contact[T], Contact[(S, T)]))(sb)

  implicit def stateLinkBuilder2Ops[T1, T2, S](p: (core.ContactWithState[T1, S], Contact[T2]))(implicit sb: BasicSystemBuilder): StateLinkBuilder2Ops[T1, T2, S] =
    new core.StateLinkBuilder2Ops(p)(sb)

  implicit def richState[S](s: StateHandle[S])(implicit sb: BasicSystemBuilder): StateOps[S] =
    new core.StateOps(s)(sb)

  implicit def contactOps[T](c: core.Contact[T])(implicit sb: BasicSystemBuilder): core.ContactOps[T] =
    new core.ContactOps(c)(sb)

  implicit def tryContactOps[T](c: core.Contact[Try[T]])(implicit sb: BasicSystemBuilder): core.TryContactOps[T] =
    new core.TryContactOps(c)(sb)

  implicit def tryFlatMapContactOps[T](c: core.Contact[Try[TraversableOnce[T]]])(implicit sb: BasicSystemBuilder): core.TryFlatMapContactOps[T] =
    new core.TryFlatMapContactOps(c)(sb)


}
