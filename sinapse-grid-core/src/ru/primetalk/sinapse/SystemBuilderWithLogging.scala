///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * ${PROJECT_NAME}
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 * (2-clause BSD license) See LICENSE
 *
 * Created: 28.06.13, zhizhelev
 */
package ru.primetalk.sinapse

import org.slf4j.LoggerFactory

trait SystemBuilderWithLogging extends SystemBuilder {

    val loggerNamePrefix = getClass.getName.replaceAllLiterally("$", "_") //"ru.primetalk.system2.contacts."


  implicit class LoggingContact[T](val c: Contact[T]) {
    require(c.name != null, "Contact name == null")
    require(c.name != "", "Contact name == \"\"")
    lazy val loggerName = loggerNamePrefix + "." + c.name
    lazy val logger = LoggerFactory.getLogger(loggerName)

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

}
