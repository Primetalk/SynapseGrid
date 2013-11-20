package ru.primetalk.synapse.core

trait SystemBuilderExtension {
	// the extended SystemBuilder
	val sb:BasicSystemBuilder
}
/** ExtensionId for a system builder. Every extension can be
 *  installed only once on the same SystemBuilder.*/
trait SystemBuilderExtensionId[T<:SystemBuilderExtension] {
	/** This method is called once for a system builder. No need to check.*/
	def extend(sb:BasicSystemBuilder):T
}