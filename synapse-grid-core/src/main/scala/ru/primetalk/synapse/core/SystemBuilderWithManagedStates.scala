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
package ru.primetalk.synapse.core

trait SystemBuilderWithManagedStates extends SystemBuilder { //WithLogging {
	
	class ManagedStateSnippet[S](val name: String, initialValue: Option[S] = None) {
		
		val state = SystemBuilderWithManagedStates.this.state[Option[S]](name + ".state", initialValue)
		val onUpdated = contact[S](name + ".onUpdated")
		val update = contact[S](name + ".update")
		update.labelNext("Option(_)").map(Option(_)).saveTo(state)
		//		def enableTracing(maxLength:Int = 20) {
		////			update.map( {(v:Any) => val s = v.toString; s.substring(0, math.min(maxLength, s.length))}, "abbreviate").
		////				trace(a=>s"$name := $a")
		//		}
		update.delay(2) >> onUpdated

	}
	
	def managedState[S](name: String, initialValue: Option[S] = None) =
		new ManagedStateSnippet[S](name, initialValue)
		
		
	implicit class ManagedRichContact[T](c: Contact[T]) {
		/** Filters out empty states.*/
		def zipWithManagedState[S](ms: ManagedStateSnippet[S], name: String = ""): Contact[(S, T)] =
			c.zipWithState(ms.state, name).labelNext("Some(state)? (state, _)").
				collect { case (Some(s), v) ⇒ (s, v) }
		def getManagedState[S](ms: ManagedStateSnippet[S], name: String = ""): Contact[S] =
			c.zipWithState(ms.state, name).labelNext("Some(state)? => state").
				collect { case (Some(s), v) ⇒ s }

		def saveToManagedState[S >: T](ms: ManagedStateSnippet[S], name: String = "") = {
			c >> ms.update
			c
		}
	}
	
	implicit class ManagedRichState[S](ms: ManagedStateSnippet[S]) {
		
		def fillFrom(c: Contact[S]) = {
			c.saveToManagedState(ms, s"fill ${ms.name} from ${c.name}")
			ms
		}
		
		def passTo(c: Contact[S]) = {
			ms.onUpdated.directly(c)
			ms
		}

		def dependsOn[D1, D2](ms1: ManagedStateSnippet[D1], ms2: ManagedStateSnippet[D2])(factory: (D1, D2) ⇒ S) = {
			val deps = depsToString(ms1, ms2)
			val depContact = contact[Boolean](s"changes from $deps")
			ms1.onUpdated.const(true, "changed").directly(depContact)
			ms2.onUpdated.const(true, "changed").directly(depContact)

			depContact.zipWithManagedState(ms1, "").zipWithManagedState(ms2, "").map { s21p ⇒
				val (s2, (s1, _)) = s21p
				factory(s1, s2)
			} saveToManagedState (ms, s"update ${ms.name} from $deps")
			
			ms
		}
		
		private def depsToString(deps: ManagedStateSnippet[_]*) = 
			deps.map(_.name).mkString(", ")
		
	}

}
