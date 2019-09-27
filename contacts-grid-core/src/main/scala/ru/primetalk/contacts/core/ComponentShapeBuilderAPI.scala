package ru.primetalk.contacts.core

import scala.language.reflectiveCalls

trait ComponentShapeBuilderAPI extends TypeSets {

  case class ComponentShape2[InputShape<:TypeSet, OutputShape <: TypeSet](inputs: InputShape, outputs: OutputShape)

  trait Contact {
    type T
  }
  sealed trait ComponentShape {
    type InputShape <: TypeSet
    type OutputShape <: TypeSet
    val inputs: InputShape
    val outputs: OutputShape
  }

  object EmptyComponentShape extends ComponentShape {
    override type InputShape = ∅
    override type OutputShape = ∅

    override val inputs: ∅ = ∅

    override val outputs: ∅ = ∅
  }

  def addInput[C<:Contact,
//    InputShape0 <: TypeSet,
//    OutputShape0 <: TypeSet,
    Comp0 <: ComponentShape](c: C, componentShape: Comp0)(
      implicit
        inputsAreContacts: EachElementIsSubtype[Contact, Comp0#InputShape],
        outputsAreContacts: EachElementIsSubtype[Contact, Comp0#OutputShape],
      InputShapeAddC: AddElement[C, Comp0#InputShape]
    ): ComponentShape{
      type InputShape = InputShapeAddC.AddElement
      type OutputShape = Comp0#OutputShape
    } = new ComponentShape {
      override type InputShape = InputShapeAddC.AddElement
      override type OutputShape = Comp0#OutputShape

      override val inputs: InputShapeAddC.AddElement = InputShapeAddC.apply(c, componentShape.inputs)

      override val outputs: Comp0#OutputShape = componentShape.outputs
    }

  trait Signal {
    type Contact
  }
  type SignalProcessor = Signal => Iterable[Signal]

  trait SignalOnContacts[Contacts <: TypeSet] {
    type C <: Contact
  }

  // typeclass for dealing with signals
  trait SignalOnContactsOps[Contacts<:TypeSet, S <: SignalOnContacts[Contacts] ] {
    def unwrap(s: S): (S#C, S#C#T)
    def get[C<:Contact](s: S, c: C)(implicit cEqSC: S#C =:= C = null, cEqSCT: S#C#T =:= C#T = null): Option[C#T] =
      if(cEqSC == null) None else Some(cEqSCT(unwrap(s)._2))
    def wrap[C<:Contact](c: C, data: C#T)(implicit cInContacts: C BelongsTo Contacts): SignalOnContacts[Contacts]
  }
  sealed trait Component {
    type Shape <: ComponentShape
    type Handler = SignalOnContacts[Shape#InputShape] => Iterable[SignalOnContacts[Shape#OutputShape]]
    val shape: Shape
    val handler: Handler
  }

  type ShapedComponent[S] = Component { type Shape = S}

  def createComponent[CompShape<: ComponentShape,
    InputContacts <: TypeSet,
    InputSignal<: SignalOnContacts[InputContacts],
    OutputSignal <: Signal
  ](f: InputSignal => Iterable[OutputSignal])(
    implicit
    evInputContacts: EachElementIsSubtype[Contact, InputContacts],
    evInputs: InputContacts ⊂ CompShape#InputShape,
    evOutputs: OutputSignal#Contact ∊ CompShape#OutputShape
  ): Component = ???


  def lift[In <: Contact, Out <: Contact](in: In, out: Out, f: In#T => Out#T)(
    implicit inSignalOnContactsOps: SignalOnContactsOps[In +: ∅, SignalOnContacts[In +: ∅]],
    outSignalOnContactsOps: SignalOnContactsOps[Out +: ∅, SignalOnContacts[Out +: ∅]]
  ):
    SignalOnContacts[In +: ∅] => Iterable[SignalOnContacts[Out +: ∅]] = signalOnContactIn =>
   {
     inSignalOnContactsOps.get(signalOnContactIn, in) match {
       case Some(dataIn) =>
         val res = f(dataIn)
         Iterable.single(outSignalOnContactsOps.wrap(out, res))
       case None =>
         Iterable.empty
     }
   }

  type Plus[Shape1 <: ComponentShape, Shape2 <: ComponentShape] = ComponentShape{
    type InputShape = Shape1#InputShape ∪ Shape2#InputShape
    type OutputShape = Shape1#OutputShape ∪ Shape2#OutputShape
  }

  def addComponent[Shape1 <: ComponentShape, Shape2 <: ComponentShape,
    Comp1 <: Component{ type Shape = Shape1 },
    Comp2 <: Component{ type Shape = Shape2 }
  ](comp1: Comp1, comp2: Comp2)(
   implicit
   ev1: UnionHelper[Shape1#InputShape, Shape2#InputShape],
   ev2: UnionHelper[Shape1#OutputShape, Shape2#OutputShape]
  ): Component { type Shape = Shape1 Plus Shape2 } = new Component {
    override type Shape = Shape1 Plus Shape2
    override val shape: Shape1 Plus Shape2 = new ComponentShape {
      type InputShape = Shape1#InputShape ∪ Shape2#InputShape
      type OutputShape = Shape1#OutputShape ∪ Shape2#OutputShape
      override val inputs: Shape1#InputShape ∪ Shape2#InputShape = ev1.out(comp1.shape.inputs, comp2.shape.inputs)
      override val outputs: Shape1#OutputShape ∪ Shape2#OutputShape = ev2.out(comp1.shape.outputs, comp2.shape.outputs)
    }
    override val handler: Handler = ???
  }
}
