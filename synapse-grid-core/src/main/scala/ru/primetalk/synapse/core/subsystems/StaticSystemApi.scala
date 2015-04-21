package ru.primetalk.synapse.core.subsystems

import ru.primetalk.synapse.core.dot.SystemRendererApi
import ru.primetalk.synapse.core.dsl.{DevNullExt, SystemBuilderDslApi}

import scala.language.{implicitConversions, reflectiveCalls}

/**
 * API for working with StaticSystem's
 * The API is influenced by Slick's cake pattern.
 *
 * @author zhizhelev, 24.03.15.
 */
trait StaticSystemApi extends DevNullExt with SystemBuilderDslApi with SystemRendererApi with BaseTypedSystemDsl {
//  /** Converts to StaticSystem an arbitrary object with method toStaticSystem.*/
//  implicit def toStaticSystem(a: {def toStaticSystem: StaticSystem}): StaticSystem = {
//    a.toStaticSystem
//  }
  /** Enriches arbitrary type with implicit converter to StaticSystem. Adds a few useful methods.*/
  implicit class RichStaticSystemType[T](t:T)(implicit cvt:T => StaticSystem){

    @inline private def system = t:StaticSystem
    /** Converts a StaticSystem to graph in dot-format.
      * Note: if you get a "missed argument ..." error, just add empty parentheses.
      * @param level how deep to dig into subsystems. Default level=0 - which means not to plot subsystem details at all.
      */
    def toDot(level: Int = 0) = SystemRenderer.staticSystem2ToDot(system, level = level)

    @deprecated("use toDot", "20.04.2015")
    def toDotAtLevel(level: Int = 0) = SystemRenderer.staticSystem2ToDot(system, level = level)


    /**
     * Constructs a system around another one.
     * It's inputs and outputs are renamed to name+"."+input.name and it's name is anew.
     * All it's state is shared.
     *
     * See also EncapsulationApi.
     */
    def encapsulate(name:String = ""):StaticSystem = {
      val s = cvt(t)
      val aName = if(name == "") s.name else name
      implicit val sb = new SystemBuilderC(aName)
      def naming(n:String) = aName+"."+n

      s.inputs.map{c =>
        sb.input(naming(c.name)) >> c
      }
      s.outputs.map{c =>
        c >> sb.output(naming(c.name))
      }
      sb.addSubsystem(s, s.privateStateHandles:_*)
      sb.toStaticSystem
    }

  }

//  implicit class RichSystemBuilder(systemBuilder: SystemBuilder)
//    extends RichStaticSystem(systemBuilder.toStaticSystem) {
//    def system = systemBuilder.toStaticSystem
//  }
//
  /**
   * Some additional information about the system. In particular,
   * one may find orphan contacts.
   */
  implicit class OrphanContactsAnalysis(system: StaticSystem) {

    val allInputContacts =
      system.components.flatMap(_.inputContacts).toSet ++ system.outputContacts

    val allOutputContacts =
      system.components.
        flatMap(_.outputContacts).toSet ++
        system.inputContacts

    /** Special "/dev/null" contacts that are intentionally ignore incoming data*/
    val nullContacts = {
      val extOpt = system.extensionOpt[ContactStyleStaticExtension]
        extOpt.map(_.styledWith(DevNullContact)).getOrElse(Iterable.empty)
    }// allOutputContacts.filter(_.contactStyle == DevNullContact)

    /** Component inputs that do not get data from anywhere. */
    val orphanComponentInputs = allInputContacts -- allOutputContacts

    /** Component outputs that are not connected anywhere. */
    val orphanComponentOutputs = allOutputContacts -- allInputContacts -- nullContacts


    /** Contacts that has only one connection either in or out. */
    val orphanContacts: Set[Contact[_]] =
      orphanComponentInputs ++
        orphanComponentOutputs
  }


  /** Recursively finds all subsystems of the system.
    * The system is the first element of the result with path = ".$systemName". */
  def collectSubsystems(system: StaticSystem): List[(String, StaticSystem)] = {
    def subsystems0(system: StaticSystem, path: String): List[(String, StaticSystem)] = {
      val path2 = path + "." + system.name
      (path2, system) :: system.staticSubsystems.flatMap(s => subsystems0(s, path2))
    }
    subsystems0(system, "")
  }

  /**
   * Recursively finds unconnected contacts
   * within the subsystems of the system.
   */
  def orphanContactsRec(system: StaticSystem): List[(String, Set[Contact[_]])] =
    collectSubsystems(system).
      map(p => (p._1, p._2.orphanContacts)).
      filterNot(_._2.isEmpty)

}
