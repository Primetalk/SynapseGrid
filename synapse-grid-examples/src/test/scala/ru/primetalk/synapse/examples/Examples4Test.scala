///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////

/**
 * SynapseGrid
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 * <p/>
 * Created: 17.07.13, zhizhelev
 */
package ru.primetalk.synapse.examples

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import ru.primetalk.synapse.core._
import ru.primetalk.synapse.examples.Examples4.SentenceSplitterBuilder

@RunWith(classOf[JUnitRunner])
class Examples4Test extends FunSuite {
  test("Dump SentenceSplitterBuilder"){
    assert(orphanContactsRec(SentenceSplitterBuilder) === List())
    SentenceSplitterBuilder.toDot().saveTo("SentenceSplitter.dot")
  }
  test("Run SentenceSplitterBuilder"){
    val s = SentenceSplitterBuilder.toStaticSystem
    val ds = s.toDynamicSystem
    val transducer = ds.toTransducer(SentenceSplitterBuilder.sentence, SentenceSplitterBuilder.allCaps)
    val result = transducer("Hello DOLLY ABC def")

    assert(result === List("DOLLY", "ABC"))
  }


}
