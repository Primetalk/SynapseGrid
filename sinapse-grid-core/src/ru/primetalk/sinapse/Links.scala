///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * SinapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 * (2-clause BSD license) See LICENSE
 *
 * Created: 28.06.13, zhizhelev
 */
package ru.primetalk.sinapse

/**
 * The Link is represented with a triple of two contacts and a linkInfo
 *
 */
case class Link[T1, T2, -TL1 >: T1, +TL2 <: T2 ](from:Contact[T1], to:Contact[T2], info: LinkInfo[TL1, TL2]) extends Named with OuterSystem {
	def toTriple = (from, to, info)
	def name = info.name
	lazy val inputContacts : Set[Contact[_]] = Set(from)
	lazy val outputContacts : Set[Contact[_]] = Set(to)
}

trait LinkInfo[-T1, +T2] extends Named
/**
 * The kind of link that does functional transformation of data.
 */
case class MapLink[T1, T2](f: T1 ⇒ T2, override val name: String)
	extends LinkInfo[T1, T2]
/**
 * The kind of link that does sequential transformation of data.
 */
case class FlatMapLink[T1, T2, TSeq <: TraversableOnce[T2]](f: T1 ⇒ TSeq, override val name: String)
	extends LinkInfo[T1, T2] 

/**
 * The kind of link that does sequential transformation of data.
 * The function itself has state that is transformed every time.
 */
case class StatefulFlatMapLink[S, T1, T2](
	f: (S, T1) ⇒ (S, Seq[T2]),
	stateHolder: StateHandle[S],
	override val name: String)
		extends LinkInfo[T1, T2] 
case class StatefulMapLink[S, T1, T2](f: (S, T1) ⇒ (S, T2),
	stateHolder:StateHandle[S],
	override val name: String)
		extends LinkInfo[T1, T2] 

case class StateZipLink[S, T1, T2 >:T1](stateHolder:StateHandle[S], override val name: String)
		extends LinkInfo[T1, (S, T2)] 

/**
 * This link can connect contacts of the same type.
 */
case class NopLink[-T1, +T2 >: T1](override val name: String)
	extends LinkInfo[T1, T2] 


/** Prioritize contacts when some data passes through this link. Processes until there are 
 *  some data on red contacts.
 *  "Fires" red contacts.*/
case class RedMapLink[T1, T2](stopContacts : Set[Contact[_]], override val name: String) extends LinkInfo [T1, T2] 
