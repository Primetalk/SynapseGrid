package ru.primetalk.synapse.core

trait SystemBuilderExtension {
	// the extended SystemBuilder
	val sb:BasicSystemBuilder
}
/** ExtensionId for a system builder. Every extension can be
 *  installed only once on the same SystemBuilder.
 *  @param extend 	This method is called once for a system builder. No need to check.
 */
final class SystemBuilderExtensionId[T<:SystemBuilderExtension](val extend: BasicSystemBuilder =>T)