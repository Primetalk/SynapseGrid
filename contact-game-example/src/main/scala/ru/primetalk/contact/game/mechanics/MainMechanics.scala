package ru.primetalk.contact.game.mechanics

import ru.primetalk.contact.game.data.Control
import ru.primetalk.contacts.core.{ComponentAlgebraDependent, MySignals, NamedContacts}

import ru.primetalk.contacts.core.UniSets._

trait MainMechanics
    extends ComponentAlgebraDependent
    with MySignals
    with NamedContacts {

  object GameComponent extends Component {
    case object ControlIn extends ContactImpl[Control]("ControlIn")
    type ControlIn = ControlIn.type

    case object MasterInput extends ContactImpl[String]("MasterIn")
    type MasterInput = MasterInput.type
    case object Player1In extends ContactImpl[String]("Player1In")
    type Player1In = Player1In.type
    case object Player2In extends ContactImpl[String]("Player2In")
    type Player2In = Player2In.type

    case object Output extends ContactImpl[String]("Output")
    type Output = Output.type

    type In = Singleton[ControlIn] ∪ Singleton[MasterInput] ∪ Singleton[Player1In] ∪ Singleton[Player2In]
    type Out = Singleton[Output]
  }



}
