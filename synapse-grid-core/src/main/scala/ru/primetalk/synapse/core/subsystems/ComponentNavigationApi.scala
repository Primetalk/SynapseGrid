package ru.primetalk.synapse.core.subsystems

import ru.primetalk.synapse.core.components.ContactsApi

/**
 * @author zhizhelev, 25.03.15.
 */
trait ComponentNavigationApi extends ContactsApi {

  type SystemPath = List[String]
  /** The system path that is reversed. One can convert to SystemPath with .reverse */
  type SystemPathReversed = List[String]

  /** Recursively finds all components that have inner structure.
    * if the component is Named then it's name is added to the path. Otherwise an empty
    * string is added.
    * */
  def subcomponents(component: Component):
  List[(SystemPathReversed, Component)] = {
    def components0(component: Component, path: SystemPathReversed):
    List[(SystemPathReversed, Component)] =
      (component.name :: path, component) :: (
        component match {
          case c: ComponentWithInternalStructure =>
            val s = c.toStaticSystem
            val name = c match {
              case n: Named => n.name;
              case _ => ""
            } //if (c.isInstanceOf[Named]) c.asInstanceOf[Named].name else ""
          val path2 = name :: path
            s.components.flatMap(c => components0(c, path2))
          case _ =>
            Nil
        }
        )
    components0(component, Nil)
  }

}
