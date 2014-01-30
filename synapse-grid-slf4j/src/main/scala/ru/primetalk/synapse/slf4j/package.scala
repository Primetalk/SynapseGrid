package ru.primetalk.synapse

import scala.language.implicitConversions
import core._


package object slf4j {

	implicit def contactToLoggingContact[T](c:Contact[T])(implicit sb:BasicSystemBuilder) = 
		new LoggingContact(c, sb.extend(SystemBuilderLoggingExtensionId).loggerNamePrefix)(basicSystemBuilderToAdvanced(sb))
	implicit val SystemBuilderLoggingExtensionId = new SystemBuilderExtensionId(new SystemBuilderLoggingExtension(_))
}
package slf4j {
	class SystemBuilderLoggingExtension(val sb:BasicSystemBuilder) extends SystemBuilderExtension {
	  val loggerNamePrefix = sb.systemName.replaceAllLiterally("$", "_") //"ru.primetalk.system2.contacts."
		
	}
	
}