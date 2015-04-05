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
import ru.primetalk.synapse.core._

import scala.language.implicitConversions
/**
 * @author zhizhelev, 28.06.13.
 */
class LoggingContact[T](val c: Contact[T], loggerNamePrefix:String)(implicit sb:SystemBuilder) {
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
