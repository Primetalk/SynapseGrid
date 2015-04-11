///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 * (2-clause BSD license) See LICENSE
 *
 * Created: 28.06.13, zhizhelev
 */
package ru.primetalk.synapse
package core

import ru.primetalk.synapse.core.dsl.SystemBuilderDslApi

import scala.collection.{GenTraversableOnce, mutable}
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.Try



//// DONE: use simple traits *Api to add this capabilities to all BasicSystemBuilders
//trait SystemBuilderImplicits extends SystemBuilderDslApi{
//
//  def sb: BasicSystemBuilder
//
//  /*
//   * Doesn't work because T2 is unknown when it is called implicitly.
//   * <pre>
//   * implicit def contactToLink[T1, T2](c1:Contact[T1]) = {
//   * val c2 = addContact(new Contact[T2](nextContactName, AuxiliaryContact))
//   * new ImplLinkBuilder(c1, c2)
//   * }
//   * </pre>
//   */
//  implicit def implDirectLinkBuilder[T1, T2 >: T1](p: (Contact[T1], Contact[T2])): DirectLinkBuilderOps[T1, T2] = new DirectLinkBuilderOps(p)(sb)
//
//  implicit def implLinkBuilder[T1, T2](c: (Contact[T1], Contact[T2])): LinkBuilderOps[T1, T2] = new LinkBuilderOps(c)(sb)
//
//  implicit def implTryLinkBuilder[T1, T2](p: (Contact[T1], Contact[Try[T2]])): TryLinkBuilderOps[T1, T2] = new TryLinkBuilderOps(p)(sb)
//
//  implicit def implRichContactPair[S, T](c: Contact[(S, T)]): ContactPairOps[S, T] = new ContactPairOps(c)(sb)
//
//  implicit def zippingLink[S, T](c: (Contact[T], Contact[(S, T)])): ZippingLinkOps[S, T] = new ZippingLinkOps[S, T](c: (Contact[T], Contact[(S, T)]))(sb)
//
//  implicit def stateLinkBuilder2Ops[T1, T2, S](p: (ContactWithState[T1, S], Contact[T2])): StateLinkBuilder2Ops[T1, T2, S] = new StateLinkBuilder2Ops(p)(sb)
//
//  implicit def richState[S](s: StateHandle[S]): StateOps[S] = new StateOps(s)(sb)
//
//  implicit def contactOps[T](c: core.Contact[T]): ContactOps[T] = new ContactOps(c)(sb)
//
//  implicit def tryContactOps[T](c: core.Contact[Try[T]]): TryContactOps[T] = new TryContactOps(c)(sb)
//
//  implicit def tryFlatMapContactOps[T](c: core.Contact[Try[TraversableOnce[T]]]): TryFlatMapContactOps[T] = new TryFlatMapContactOps(c)(sb)
//
//}
// TODO: macros like: `state counterS:Int = 0` and `contact myContact:String`


//trait SystemBuilderAdv  {//SystemBuilderImplicits with
//  def sb: BasicSystemBuilder
//
//  def nextContactName =
//    sb.extend(ru.primetalk.synapse.core.AuxContactNumberingExtId).nextContactName

//  /**
//   * Defines the sequence of labels to be used for superscription of links.
//   */
//  def labels(labels: String*) = {
//    sb.extend(ru.primetalk.synapse.core.LabellingExtId).labels(labels: _*)
//    this
//  }
//
//  private[synapse] def nextLabel(userProvidedLabel: String, defaultLabel: => String): String = {
//    val lsb = sb.extend(ru.primetalk.synapse.core.LabellingExtId)
//    (userProvidedLabel, lsb.proposedLabels) match {
//      case ("", List()) ⇒ defaultLabel
//      case ("", head :: tail) ⇒
//        sb.assertWritable()
//        lsb.proposedLabels = tail
//        head
//      case (label, _) => label
//    }
//  }

//  /**
//   * Create contact and add it to the builder
//   */
//  def contact[T](name: String) =
//    core.contact[T](name)

//  def auxContact[T] = sb.auxContact[T]

//  /**
//   * Special contact for consuming unnecessary data values.
//   */
//  def devNull = core.devNull




//}


//class SystemBuilderAdvC(val sb: BasicSystemBuilder) extends SystemBuilderAdv
