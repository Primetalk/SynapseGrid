///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev
 *
 * Created: 06.12.13, zhizhelev
 */
package ru.primetalk.synapse.core

import scala.language.implicitConversions

object implicits {
  implicit def implDirectLinkBuilder[T1, T2 >: T1](p: (Contact[T1], Contact[T2]))(implicit sb:BasicSystemBuilder): DirectLinkBuilderOps[T1, T2] = new DirectLinkBuilderOps(p)(sb)
  implicit def implLinkBuilder[T1, T2](c: (Contact[T1], Contact[T2]))(implicit sb:BasicSystemBuilder): LinkBuilderOps[T1, T2] = new LinkBuilderOps(c)(sb)

  implicit def implRichContactPair[S, T](c: Contact[(S, T)])(implicit sb:BasicSystemBuilder): ContactPairOps[S, T] = new ContactPairOps(c)(sb)
  implicit def zippingLink[S, T](c: (Contact[T], Contact[(S, T)]))(implicit sb:BasicSystemBuilder): ZippingLinkOps[S, T] = new ZippingLinkOps[S, T](c: (Contact[T], Contact[(S, T)]))(sb)

  implicit def stateLinkBuilder2Ops[T1, T2, S](p: (ContactWithState[T1, S], Contact[T2]))(implicit sb:BasicSystemBuilder): StateLinkBuilder2Ops[T1, T2, S] = new StateLinkBuilder2Ops(p)(sb)

  implicit def richState[S](s: StateHandle[S])(implicit sb:BasicSystemBuilder): StateOps[S] = new StateOps(s)(sb)
  implicit def contactOps[T](c: Contact[T])(implicit sb:BasicSystemBuilder): ContactOps[T] = new ContactOps(c)(sb)

}
