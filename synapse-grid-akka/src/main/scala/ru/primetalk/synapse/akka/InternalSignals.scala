/////////////////////////////////////////////////////
// Речевой портал                                  //
// © ООО «Праймтолк», 2011-2013                    //
// Авторы: Жижелев А.А., Нехаев А.Р., Попов П.А.   //
// Все права принадлежат компании ООО «Праймтолк». //
/////////////////////////////////////////////////////
/**
 * Speech portal
 * © Primetalk Ltd., 2011-2013.
 * Authors: Zhizhelev A., Nehaev A., Popov P.
 * All rights reserved.
 * Created: 13.02.2013
 */
package ru.primetalk.synapse.akka


import ru.primetalk.synapse.core.Signal
import scala.language.postfixOps

/**
  * Signals from subsystems.
  *
  * @param path can be used to store the position of the system (the path to the system)
  *                  or a signal processor for output signals.
  *                  It is stored by parent system and is handled exclusively by parent.
  *
  */
case class InternalSignals(path:List[String], list: List[Signal[_]])

///** Signals from external systems.*/
// @deprecated("Signals should come one by one", "01.07.2013")
//case class Signals(list: List[Signal[_]])


