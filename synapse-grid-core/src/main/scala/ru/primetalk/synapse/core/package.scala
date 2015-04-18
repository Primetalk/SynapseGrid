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
 * Created: 30.06.13, zhizhelev
 */
package ru.primetalk.synapse

import ru.primetalk.synapse.core.components.SignalsApi
import ru.primetalk.synapse.core.dot.{SystemRendererApi, DotUtilsApi}
import ru.primetalk.synapse.core.dsl._
import ru.primetalk.synapse.core.runtime.{SystemConvertingApi, TrellisProducerApi, RichSimpleSignalProcessorApi}
import ru.primetalk.synapse.core.subsystems.{StaticSystemApi, ComponentNavigationApi, EncapsulationApi, BaseTypedSystemApi}

import scala.language.implicitConversions

package object core
  extends SystemBuilderApi
  with TryDsl
  with StaticSystemApi
  with RichSimpleSignalProcessorApi
  with TrellisProducerApi
  with ComponentNavigationApi
  with ExceptionHandlingExt
  with SignalsApi
  with BaseTypedSystemDsl
  with DotUtilsApi
  with ContactsDsl
  with EncapsulationApi
  with SystemBuilderDsl2Api
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
