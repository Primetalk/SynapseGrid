package ru.primetalk.contacts.core

trait ComponentShapeBuilderAPI extends TypeSets {
  type Contact[T]
  sealed trait ComponentShape {
    type InputShape <: TypeSet
    type OutputShape <: TypeSet
    def inputs: InputShape
    def outputs: OutputShape
  }

  object EmptyComponentShape extends ComponentShape {
    override type InputShape = ∅
    override type OutputShape = ∅

    override def inputs: ∅ = ∅

    override def outputs: ∅ = ∅
  }

  def addInput[T, C<:Contact[T],
//    InputShape0 <: TypeSet,
//    OutputShape0 <: TypeSet,
    Comp0 <: ComponentShape](c: C, componentShape: Comp0)(
      implicit
        inputsAreContacts: EachElementIsSubtype[Contact[T], Comp0#InputShape],
        outputsAreContacts: EachElementIsSubtype[Contact[T], Comp0#OutputShape],
      InputShapeAddC: AddWrapper[C, Comp0#InputShape]
    ): ComponentShape{
      type InputShape = InputShapeAddC.AuxPlus
      type OutputShape = Comp0#OutputShape
    } = new ComponentShape {
      override type InputShape = InputShapeAddC.AuxPlus
      override type OutputShape = Comp0#OutputShape

      override def inputs: InputShapeAddC.AuxPlus = InputShapeAddC.auxPlus(c, componentShape.inputs)

      override def outputs: Comp0#OutputShape = componentShape.outputs
    }
}
