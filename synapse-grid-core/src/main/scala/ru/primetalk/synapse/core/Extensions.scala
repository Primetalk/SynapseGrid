package ru.primetalk.synapse.core

/** */
trait SystemBuilderExtension {
  // the extended SystemBuilder
  val sb: BasicSystemBuilder
  /** Opportunity for extension to hook into method
    * SystemBuilder#toStaticSystem".
    * It can also add some information to extensions map. */
  def postProcess(s:StaticSystem):StaticSystem = s
}

/** ExtensionId for a system builder. Every extension can be
  * installed only once on the same SystemBuilder. An instance of the extension
  * is created by the extend method.
  * @param extend 	This method is called once for a system builder. No need to check.
  */
final class SystemBuilderExtensionId[T <: SystemBuilderExtension](val extend: BasicSystemBuilder => T)

/** An extension that can be obtained from an instance of StaticSystem.
  * It will be automatically added when asked for.
  *
  * The extension can contain some additional state for system processing.*/
trait StaticSystemExtension

/** ExtensionId for a StaticSystem. Every extension can be
  * installed only once on the same StaticSystem.
  * However,
  * After creation the extension instance is added to StaticSystem.
  */
trait StaticSystemExtensionId[+T]