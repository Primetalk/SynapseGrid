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
package ru.primetalk.synapse.slf4j

import ru.primetalk.synapse.core.syntax._

import scala.language.implicitConversions


@deprecated("use Slf4jApi and extension", "01.04.2015")
trait SystemBuilderWithLogging extends SystemBuilder {

  val loggerNamePrefix = getClass.getName.replace("$", "_") //"ru.primetalk.system2.contacts."


  implicit def contactToLoggingContact[T](c:Contact[T]): LoggingContact[T] =
    new LoggingContact(c, loggerNamePrefix)(this)
}


@deprecated("use Slf4jApi and extension", "01.04.2015")
class SystemBuilderWithLoggingC(name:String) extends SystemBuilderWithLogging{
  systemName = name

  override
  val loggerNamePrefix = name
}