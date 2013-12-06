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

import ru.primetalk.synapse.core.{Signal, Contact, DynamicSystem}
import rx.lang.scala.{Observable, Observer}
import rx.lang.scala.subjects.PublishSubject

class DynamicSystemRx(ds:DynamicSystem){
  private
  val spRx = new SimpleSignalProcessorRx(ds.receive)

  private
  def inputRx[T](c:Contact[T]):Observer[T] = {
    val s = PublishSubject[T]()
    s.map(data => Signal(c, data)).subscribe(spRx.rxInput)
    s
  }

  private
  def outputRx[T](contact:Contact[T]):Observable[T] = {
    val s = PublishSubject[T]()
    spRx.rxOutput.filter(_.contact == contact).map(_.data.asInstanceOf[T]).subscribe(s)
    s
  }
  private
  val rxInputs = ds.inputContacts.map(c => (c, inputRx(c))).toMap[Contact[_], Observer[_]]

  private
  val rxOutputs = ds.outputContacts.map(c => (c, outputRx(c))).toMap[Contact[_], Observable[_]]

  def rxInput[T](c:Contact[T]):Observer[T] = rxInputs(c).asInstanceOf[Observer[T]]
  def rxOutput[T](c:Contact[T]):Observable[T] = rxOutputs(c).asInstanceOf[Observable[T]]
}