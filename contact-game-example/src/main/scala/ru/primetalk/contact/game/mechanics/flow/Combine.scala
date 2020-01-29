package ru.primetalk.contact.game.mechanics.flow

import java.util.concurrent.atomic.AtomicReference

import ru.primetalk.contacts.core.UniSets._
import ru.primetalk.contacts.core.{ComponentAlgebraDependent, MySignals, NamedContacts}

trait Combine
    extends ComponentAlgebraDependent
    with MySignals
    with NamedContacts {


  class DelayedCombineComponent[T1, T2, R] extends Component{
    case object left extends ContactImpl[T1]("left")
    case object right extends ContactImpl[T2]("right")
    case object out extends ContactImpl[R]("out")

    override type In = left.AsUniSet ∪ right.AsUniSet
    override type Out = out.AsUniSet
  }



  def defineCombineHandler[T1, T2, R, C <: DelayedCombineComponent[T1, T2, R]](
      component: C
  )(
      body: (T1, T2) => R
  ): HandlerOf[component.type] = {

    type StateType = Option[Either[T1, T2]]
    val state = new AtomicReference[StateType](None) //Woops, it shouldn't be ...

    def recalcState(state: StateType, left: Option[T1], right: Option[T2]): Either[R, StateType] = {
      val newState = left.map(Left(_)).orElse(right.map(Right(_)))
      state match {
        case Some(Left(t1)) =>
          right.map(body.curried(t1)).toLeft[StateType](Some(Left(t1)))
        case Some(Right(t2)) =>
          left.map(t1 => body(t1, t2)).toLeft[StateType](Some(Right(t2)))
        case None =>
          Right(newState)
      }
    }

    val clearStateAndReturn: R => Option[Signal[component.Out]] = r => {
      state.set(None)
      val outSignal = new MySignalOnContact(component.out)(r)
      val inputSignal = signal[component.Out](outSignal)
      Some(inputSignal)
    }

    val updateStateOnly: StateType => Option[Signal[component.Out]] = st => {
      state.set(st)
      None
    }

    defineHandlerOf[component.type]{
      d =>
        val left = d.unwrap(component.left)
        val right = d.unwrap(component.right)
        val res = recalcState(state.get, left, right)
        res.fold(clearStateAndReturn, updateStateOnly).toSeq
    }
  }
}
