package ru.primetalk.synapse.core.impl

import ru.primetalk.synapse.core._

import scala.collection.immutable.Iterable
import scala.collection.mutable.ListBuffer

/**
 * An extension to add style information to contacts. It is used primarily by
 * the SystemRenderer to convert StaticSystem to .dot-file
 * @author zhizhelev, 05.04.15.
 */
trait ContactStyleExt {
  trait ContactStyle

  /** Default style for contact.*/
  case object NormalContact extends ContactStyle
//
//  case object StateContact extends ContactStyle

  class ContactStyleExtension(val sb: BasicSystemBuilder) extends SystemBuilderExtension{
    private[ContactStyleExt]
    var styles = List[(Contact[_], ContactStyle)]()

    /** Opportunity for extension to hook into method
      * SystemBuilder#toStaticSystem".
      * It can also add some information to extensions map. */
    override def postProcess(s: StaticSystem): StaticSystem =
      s.copy(extensions = s.extensions.updated(ContactStyleExtId, styles.toMap.withDefaultValue(NormalContact)))

  }
  implicit val ContactStyleExtId = new SystemBuilderExtensionId(new ContactStyleExtension(_))

  implicit class ContactStyled[T](c:Contact[T]){
    def styled(s:ContactStyle)(implicit sb:BasicSystemBuilder) = {
      val ext = sb.extend(ContactStyleExtId)
      ext.styles = (c,s) :: ext.styles
      c
    }
  }
  implicit class StyledSystem(s:StaticSystem){
    def styles:Map[Contact[_], ContactStyle] = s.extensions.get(ContactStyleExtId).
      map(e => e.asInstanceOf[Map[Contact[_], ContactStyle]]).getOrElse(Map())

    def style(c:Contact[_]):ContactStyle = {
      s.extensions.get(ContactStyleExtId).
        map(e => e.asInstanceOf[Map[Contact[_], ContactStyle]](c)).
        getOrElse(NormalContact)
    }
    def styledWith(s:ContactStyle): Iterable[Contact[_]] = styles.filter(_._2 == s).map(_._1)
  }
}
