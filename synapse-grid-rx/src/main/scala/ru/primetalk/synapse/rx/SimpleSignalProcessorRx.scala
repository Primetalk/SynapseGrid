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
package ru.primetalk.synapse.rx

import ru.primetalk.synapse.core._
import rx.lang.scala.subjects.PublishSubject
import ru.primetalk.synapse.core.Signal
import rx.lang.scala.{Observable, Observer}
import scala.util.Try

class SimpleSignalProcessorRx(sp:SimpleSignalProcessor){
  private
  val rxInputSubject = PublishSubject[Signal[_]]()
  private
  val rxOutputSubject = PublishSubject[Signal[_]]()
  rxInputSubject.subscribe{s =>
    val res = Try(sp(s))
    res.foreach(_.foreach(rxOutputSubject.onNext))
    res.recover{ case (e:Throwable) => rxOutputSubject.onError(e)}
  }

  val rxInput:Observer[Signal[_]] = rxInputSubject
  val rxOutput:Observable[Signal[_]] = rxOutputSubject
}