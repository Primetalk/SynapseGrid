package ru.primetalk.synapse.core.impl

import ru.primetalk.synapse.core.{StateHandle, Contact}

import scala.util.Try
import scala.language.implicitConversions

/**
 * @author zhizhelev, 25.03.15.
 */
trait SystemBuilderImplicitsApi extends SystemBuilderApi with SystemBuilderDslApi with SwitcherBuilderApi {

  def extendBasicSystemBuilder[T <: SystemBuilderExtension]( implicit sb: SystemBuilder,
        extensionInstance: SystemBuilderExtensionId[T]): T =
    sb.extend(extensionInstance)

  implicit def implLinkBuilder[T1, T2](c: (Contact[T1], Contact[T2]))(implicit sb: SystemBuilder): LinkBuilderOps[T1, T2] =
    new LinkBuilderOps(c)(sb)

  implicit def implTryLinkBuilder[T1, T2](p: (Contact[T1], Contact[Try[T2]]))(implicit sb: SystemBuilder): TryLinkBuilderOps[T1, T2] =
    new TryLinkBuilderOps(p)(sb)

  implicit def implDirectLinkBuilder[T1, T2 >: T1](p: (Contact[T1], Contact[T2]))(implicit sb: SystemBuilder): DirectLinkBuilderOps[T1, T2] =
    new DirectLinkBuilderOps(p)(sb)

  implicit def implRichContactPair[S, T](c: Contact[(S, T)])(implicit sb: SystemBuilder): ContactPairOps[S, T] =
    new ContactPairOps(c)(sb)

  implicit def zippingLink[S, T](c: (Contact[T], Contact[(S, T)]))(implicit sb: SystemBuilder): ZippingLinkOps[S, T] =
    new ZippingLinkOps[S, T](c: (Contact[T], Contact[(S, T)]))(sb)

  implicit def stateLinkBuilder2Ops[T1, T2, S](p: (ContactWithState[T1, S], Contact[T2]))(implicit sb: SystemBuilder): StateLinkBuilder2Ops[T1, T2, S] =
    new StateLinkBuilder2Ops(p)(sb)

  implicit def richState[S](s: StateHandle[S])(implicit sb: SystemBuilder): StateOps[S] =
    new StateOps(s)(sb)

  implicit def contactOps[T](c: Contact[T])(implicit sb: SystemBuilder): ContactOps[T] =
    new ContactOps(c)(sb)

  implicit def tryContactOps[T](c: Contact[Try[T]])(implicit sb: SystemBuilder): TryContactOps[T] =
    new TryContactOps(c)(sb)

  implicit def tryFlatMapContactOps[T](c: Contact[Try[TraversableOnce[T]]])(implicit sb: SystemBuilder): TryFlatMapContactOps[T] =
    new TryFlatMapContactOps(c)(sb)


}
