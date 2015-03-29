package ru.primetalk.synapse.core

trait SystemBuilderExtension {
  // the extended SystemBuilder
  val sb: BasicSystemBuilder
  /** Opportunity for extension to hook into method
    * SystemBuilder#toStaticSystem"*/
  def postProcess(s:StaticSystem):StaticSystem = s
}

/** ExtensionId for a system builder. Every extension can be
  * installed only once on the same SystemBuilder. An instance of the extension
  * is created by the extend method.
  * @param extend 	This method is called once for a system builder. No need to check.
  */
final class SystemBuilderExtensionId[T <: SystemBuilderExtension](val extend: BasicSystemBuilder => T)