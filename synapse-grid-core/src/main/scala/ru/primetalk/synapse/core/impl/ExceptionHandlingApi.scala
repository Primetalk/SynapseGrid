package ru.primetalk.synapse.core.impl

import ru.primetalk.synapse.core._

/**
 * @author zhizhelev, 25.03.15.
 */
trait ExceptionHandlingApi {

  /** The type of a handler that will handle exceptions during signal processing.
    * If the exception is recoverable, then the handler should provide a new Context
    * for further processing.
    * If not recoverable - throw some exception (or rethrow the original one).
    * */
  type UnhandledProcessingExceptionHandler = (Throwable, String, Signal[_], Context) => Context

  val defaultUnhandledExceptionHandler : UnhandledProcessingExceptionHandler = (e, name, signal, context) => e match {
    case e:Exception =>
      val message: String =
        s"Exception ${e.getClass.getSimpleName} in handler during processing '$signal' in system '$name'.\n" +
          s"Context value before processing:\n" + context.mkString("\n")
      throw new RuntimeException(message,e)
    case other =>
      throw other
  }
  val rethrowUnhandledExceptionHandler : UnhandledProcessingExceptionHandler = (e, name, signal, context) => e match {
    case any =>
      throw any
  }

}
