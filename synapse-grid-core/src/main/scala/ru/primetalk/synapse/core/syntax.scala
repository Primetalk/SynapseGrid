package ru.primetalk.synapse.core

import ru.primetalk.synapse.core.dot.{FilesApi, SystemRendererApi}
import ru.primetalk.synapse.core.dsl._
import ru.primetalk.synapse.core.ext._
import ru.primetalk.synapse.core.runtime.{SignalProcessingDsl, SystemConvertingApi}
import ru.primetalk.synapse.core.subsystems._

object syntax extends SystemBuilderApi
  with TryDsl
  with StaticSystemApi
  with SignalProcessingDsl
  with ComponentNavigationApi
  with ExceptionHandlingExt
  with SignalsApi
  with BaseTypedSystemDsl
  with FilesApi
  with ContactsDsl
  with EncapsulationApi
  with ContinuationDsl
  with AccumulationDsl
  with ManagedStatesDsl
  with AutomataDsl
  with SystemRendererApi
  with AuxNumberingExt
  with NextLabelExt
  with ContactStyleExt
  with DevNullExt
  with ContactsIndexExt
  with SystemConvertingApi
  with BaseTypedSystemApi
  with SwitcherDsl
