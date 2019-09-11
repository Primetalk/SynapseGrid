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
import ru.primetalk.synapse.core.dot.{SystemRendererApi, FilesApi}
import ru.primetalk.synapse.core.dsl._
import ru.primetalk.synapse.core.ext._
import ru.primetalk.synapse.core.runtime.{SystemConvertingApi, SignalProcessingDsl}
import ru.primetalk.synapse.core.subsystems._

import scala.language.implicitConversions

package object core
  extends SystemBuilderApi
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
  with Switcher2Dsl
