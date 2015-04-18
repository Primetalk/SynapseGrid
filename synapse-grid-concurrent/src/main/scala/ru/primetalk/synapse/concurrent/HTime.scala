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
 * Created: 27.09.13, zhizhelev
 */
package ru.primetalk.synapse.concurrent

import ru.primetalk.synapse.core.SignalCollection
/**
 * The sequence from some initial time.
 */
case class HTime(previous:Option[HTime], index:Int) {
  val trellisTime:Int = previous.map(_.trellisTime + 1).getOrElse(0)
  def next(i:Int) = HTime(Some(this), i)
}


object HTime {
  implicit val timeOrderingInstance = new Ordering[HTime] {
    def compare(x: HTime, y: HTime): Int = {
      val tc = x.trellisTime - y.trellisTime
      if (tc != 0)
        tc
      else (x,y) match {
        case (HTime(None,ix),HTime(None,iy)) =>
          ix-iy
        case (HTime(Some(px),ix),HTime(Some(py),iy)) if px eq py=>
          ix-iy
        case (HTime(Some(px),ix),HTime(Some(py),iy)) =>
          val pc = compare(px, py)
          if(pc != 0)
            pc
          else
            throw new IllegalStateException("Impossible case")
        case _ =>
          throw new IllegalStateException(s"Times are incomparable ($x, $y")
      }
    }
  }
}


/** Associate a value with the time moment.
  * @param time the time moment
  * @param value associated value.
  **/
case class AtTime[+T](time:HTime, value: T)


object AtTime {
  private val timeOrderingInstance = new Ordering[AtTime[_]] {
    def compare(x: AtTime[_], y: AtTime[_]): Int =
      HTime.timeOrderingInstance.compare( x.time, y.time)
  }

  /** Lexicographical ordering. */
  implicit def timeOrdering[T] = timeOrderingInstance.asInstanceOf[Ordering[AtTime[T]]]
  def placeAfter[T](time:HTime, list:SignalCollection[T]):SignalCollection[AtTime[T]] =
    list.zipWithIndex.map{ case (s, i) =>
      AtTime(time.next(i), s)
    }
}

