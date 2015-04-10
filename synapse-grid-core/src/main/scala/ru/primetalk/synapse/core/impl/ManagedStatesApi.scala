///////////////////////////////////////////////////////////////
// © ООО «Праймтолк», 2011-2013                              //
// Все права принадлежат компании ООО «Праймтолк».           //
///////////////////////////////////////////////////////////////
/**
 * ${PROJECT_NAME}
 * © Primetalk Ltd., 2013.
 * All rights reserved.
 * Authors: A.Zhizhelev, A.Nehaev, P. Popov
 *
 * Created: 24.07.13, zhizhelev
 */
package ru.primetalk.synapse.core.impl

import ru.primetalk.synapse.core.Contact

import scala.language.implicitConversions

/** Managed states are not accessible directly. Instead the state can be overwritten by sending some data
  * on a special contact.
  * On every state change a signal appears on a contact onUpdated
  * */
trait ManagedStatesApi extends BasicSystemBuilderApi with ContactsApi with SystemBuilderDslApi {

  class ManagedStateSnippet[S](val name: String, initialValue: Option[S] = None)(implicit sb:SystemBuilder) {

    val state = sb.state[Option[S]](name + ".state", initialValue)
    val onUpdated = contact[S](name + ".onUpdated")
    val update = contact[S](name + ".update")
    update.labelNext("Option(_)").map(Option(_)).saveTo(state)
    update.delay(2) >> onUpdated

  }

  def managedState[S](name: String, initialValue: Option[S] = None)(implicit sb:SystemBuilder):ManagedStateSnippet[S] =
    new ManagedStateSnippet[S](name, initialValue)


  implicit class ManagedRichContact[T](c: Contact[T])(implicit sb:SystemBuilder) {
    /** Filters out empty states. */
    def zipWithManagedState[S](ms: ManagedStateSnippet[S], name: String = ""): Contact[(S, T)] =
      c.zipWithState(ms.state, name).labelNext("Some(state)? (state, _)").
        collect { case (Some(s), v) ⇒
        (s:S, v)
      }

    def getManagedState[S](ms: ManagedStateSnippet[S], name: String = ""): Contact[S] =
      c.zipWithState(ms.state, name).labelNext("Some(state)? => state").
        collect { case (Some(s), v) ⇒ s:S}

    def saveToManagedState[S >: T](ms: ManagedStateSnippet[S], name: String = "") = {
      c >> ms.update
      c
    }
  }
  implicit def managedRichState[S](ms: ManagedStateSnippet[S])(implicit sb:SystemBuilder):ManagedRichState[S]  = new ManagedRichState[S](ms)(sb)

  class ManagedRichState[S](ms: ManagedStateSnippet[S])(implicit sb:SystemBuilder) {

    def >>(c: Contact[S]) = ms.onUpdated >> c

    def >>:(c: Contact[S]) = {
      c >> ms.update
      ms.onUpdated
    }

    def fillFrom(c: Contact[S]) = {
      new ManagedRichContact(c).saveToManagedState(ms, s"fill ${ms.name} from ${c.name}")
      ms
    }

    def passTo(c: Contact[S]) = {
      ms.onUpdated.directly(c)
      ms
    }

    def dependsOn[D1](ms1: ManagedStateSnippet[D1])(factory: (D1) ⇒ S) = {
      val deps = depsToString(ms1)

      ms1.onUpdated. //getManagedState(ms1).
        map(factory).
        saveToManagedState(ms, s"update ${ms.name} from $deps")
      ms
    }

    def dependsOn[D1, D2](ms1: ManagedStateSnippet[D1], ms2: ManagedStateSnippet[D2])(factory: (D1, D2) ⇒ S) = {
      val deps = depsToString(ms1, ms2)
      val depContact = contact[Any](s"invalidateMS${ms.name}")
      ms1.onUpdated >> depContact
      ms2.onUpdated >> depContact

      depContact.getManagedState(ms1).zipWithManagedState(ms2).map {
        case (s2, s1) ⇒
          factory(s1, s2)
      } saveToManagedState(ms, s"update ${ms.name} from $deps")

      ms
    }

    def dependsOn[D1, D2, D3](ms1: ManagedStateSnippet[D1], ms2: ManagedStateSnippet[D2], ms3: ManagedStateSnippet[D3])(factory: (D1, D2, D3) ⇒ S) = {
      val deps = depsToString(ms1, ms2, ms3)
      val depContact = contact[Any](s"invalidateMS${ms.name}")
      ms1.onUpdated >> depContact
      ms2.onUpdated >> depContact
      ms3.onUpdated >> depContact

      depContact.getManagedState(ms1).zipWithManagedState(ms2).zipWithManagedState(ms3).map {
        case (s3, (s2, s1)) ⇒
          factory(s1, s2, s3)
      } saveToManagedState(ms, s"update ${ms.name} from $deps")

      ms
    }

    def dependsOn[D1, D2, D3, D4](ms1: ManagedStateSnippet[D1], ms2: ManagedStateSnippet[D2], ms3: ManagedStateSnippet[D3], ms4: ManagedStateSnippet[D4])(factory: (D1, D2, D3, D4) ⇒ S) = {
      val deps = depsToString(ms1, ms2, ms3, ms4)
      val depContact = contact[Any](s"invalidateMS${ms.name}")
      ms1.onUpdated >> depContact
      ms2.onUpdated >> depContact
      ms3.onUpdated >> depContact
      ms4.onUpdated >> depContact

      depContact.getManagedState(ms1).zipWithManagedState(ms2).zipWithManagedState(ms3).zipWithManagedState(ms4).map {
        case (s4, (s3, (s2, s1))) ⇒
          factory(s1, s2, s3, s4)
      } saveToManagedState(ms, s"update ${ms.name} from $deps")

      ms
    }


    private def depsToString(deps: ManagedStateSnippet[_]*) =
      deps.map(_.name).mkString(", ")

  }

}
