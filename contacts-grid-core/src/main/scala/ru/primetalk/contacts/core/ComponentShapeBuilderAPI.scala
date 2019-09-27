package ru.primetalk.contacts.core

import scala.language.reflectiveCalls

trait ComponentShapeBuilderAPI extends Signals {


  case class ComponentShape2[InputShape<:TypeSet, OutputShape <: TypeSet](inputs: InputShape, outputs: OutputShape)


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

  def addInput[C<:Contact, Comp0 <: ComponentShape](c: C, componentShape: Comp0)(
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

  sealed trait Component {
    type Shape <: ComponentShape
    type Handler = SignalOnContacts[Shape#InputShape] => Iterable[SignalOnContacts[Shape#OutputShape]]
    val shape: Shape
    val handler: Handler
  }

  type ShapedComponent[S] = Component { type Shape = S}
//
//  def createComponent[CompShape<: ComponentShape,
//    InputContacts <: TypeSet,
//    InputSignal<: SignalOnContacts[InputContacts],
//    OutputSignal <: Signal
//  ](f: InputSignal => Iterable[OutputSignal])(
//    implicit
//    evInputContacts: EachElementIsSubtype[Contact, InputContacts],
//    evInputs: InputContacts ⊂ CompShape#InputShape,
//    evOutputs: OutputSignal#Contact ∊ CompShape#OutputShape
//  ): Component = ???


  type Plus[Shape1 <: ComponentShape, Shape2 <: ComponentShape] = ComponentShape{
    type InputShape = Shape1#InputShape ∪ Shape2#InputShape
    type OutputShape = Shape1#OutputShape ∪ Shape2#OutputShape
  }

  // concatenates components so that they have concatenated inputs, outputs and handlers.
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
