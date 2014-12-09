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

import org.slf4j.LoggerFactory
import ru.primetalk.synapse.core.{Contact, Signal, SystemBuilder, SystemBuilderAdv}

import scala.language.implicitConversions

///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
trait SystemBuilderWithLogging extends SystemBuilder {

  val loggerNamePrefix = getClass.getName.replaceAllLiterally("$", "_") //"ru.primetalk.system2.contacts."


  implicit def contactToLoggingContact[T](c:Contact[T]): LoggingContact[T] =new LoggingContact(c, loggerNamePrefix)(this)
}
class LoggingContact[T](val c: Contact[T], loggerNamePrefix:String)(implicit sb:SystemBuilderAdv) {
    require(c.name != null, "Contact name == null")
    require(c.name != "", "Contact name == \"\"")
    lazy val loggerName = loggerNamePrefix + "." + c.name
    lazy val logger = LoggerFactory.getLogger(loggerName)

    import sb._
    /** Enables tracing on the contact. */
    def info(f: Signal[T] ⇒ String = "" + _) = {
      c.foreach(data ⇒ logger.info(f(Signal(c, data))), "INFO: " + loggerName)
      c
    }
    /** Enables tracing on the contact. */
    def trace(f: Signal[T] ⇒ String = "" + _) = {
      c.foreach(data ⇒ logger.trace(f(Signal(c, data))), "TRACE: " + loggerName)
      c
    }

    /** Enables tracing on the contact. */
    def debug(f: Signal[T] ⇒ String = "" + _) = {
      c.foreach(data ⇒ logger.debug(f(Signal(c, data))), "DEBUG: " + loggerName)
      c
    }


  }

class SystemBuilderWithLoggingC(name:String) extends SystemBuilderWithLogging{
  systemName = name

  override
  val loggerNamePrefix = name
}