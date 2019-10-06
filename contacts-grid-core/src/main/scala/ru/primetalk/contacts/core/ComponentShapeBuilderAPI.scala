package ru.primetalk.contacts.core

import scala.language.reflectiveCalls

import TypeSets._

trait ComponentShapeBuilderAPI extends Signals {


  case class ComponentShape2[InputShape <: TypeSet, OutputShape <: TypeSet](inputs: InputShape, outputs: OutputShape)


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
      type InputShape = InputShapeAddC.Sum
      type OutputShape = Comp0#OutputShape
    } = new ComponentShape {
      override type InputShape = InputShapeAddC.Sum
      override type OutputShape = Comp0#OutputShape

      override val inputs: InputShapeAddC.Sum = InputShapeAddC.apply(c, componentShape.inputs)

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

//
//  type Plus[Shape1 <: ComponentShape, Shape2 <: ComponentShape] = ComponentShape{
//    type InputShape = Shape1#InputShape ∪ Shape2#InputShape
//    type OutputShape = Shape1#OutputShape ∪ Shape2#OutputShape
//  }

  class unwrapSignal[A <: TypeSet, B <: TypeSet, C <: UnionHelper[A,B]](val unionHelper: C) {
    def apply(a: A, b: B, signalOnContacts: SignalOnContacts[C#Out]): (Option[SignalOnContacts[A]], Option[SignalOnContacts[B]]) =
      (
        if (a.contains0(signalOnContacts.contact)) Some(signalOnContacts.asInstanceOf[SignalOnContacts[A]]) else None,
        if (b.contains0(signalOnContacts.contact)) Some(signalOnContacts.asInstanceOf[SignalOnContacts[B]]) else None
      )
  }

  // concatenates components so that they have concatenated inputs, outputs and handlers.
  def parallelAddComponent[Shape1 <: ComponentShape, Shape2 <: ComponentShape,
    Comp1 <: Component{ type Shape = Shape1 },
    Comp2 <: Component{ type Shape = Shape2 }
  ](comp1: Comp1, comp2: Comp2)(
   implicit
   inputShapesUnion: UnionHelper[Shape1#InputShape, Shape2#InputShape],
   outputShapesUnion: UnionHelper[Shape1#OutputShape, Shape2#OutputShape]
  ): Component {
    type Shape = ComponentShape {
      type InputShape = inputShapesUnion.Out
      type OutputShape = outputShapesUnion.Out
    }
  } = new Component {
    override type Shape = ComponentShape {
      type InputShape = inputShapesUnion.Out
      type OutputShape = outputShapesUnion.Out
    }
    override val shape: Shape = new ComponentShape {
      type InputShape = inputShapesUnion.Out
      type OutputShape = outputShapesUnion.Out
      override val inputs: InputShape = inputShapesUnion.apply(comp1.shape.inputs, comp2.shape.inputs)
      override val outputs: OutputShape = outputShapesUnion.apply(comp1.shape.outputs, comp2.shape.outputs)
    }
    override val handler: Handler = signalOnContacts => {
      val (s1, s2) = (new unwrapSignal[Shape1#InputShape, Shape2#InputShape, inputShapesUnion.type](inputShapesUnion)).
        apply(comp1.shape.inputs, comp2.shape.inputs, signalOnContacts )
      val res = s1.toIterable.flatMap(comp1.handler) ++ s2.toIterable.flatMap(comp2.handler)
      res.asInstanceOf[Iterable[SignalOnContacts[Shape#OutputShape]]]
    }
  }

  // set of contacts that belong to a system.
  sealed trait BreadcrumbShape {
    type OutputShape <: TypeSet
    type InputShape <: TypeSet
    val outputs: OutputShape
    val inputs: InputShape
  }
  // In Out

  trait Breadcrumb {
    type Shape <: BreadcrumbShape
  }
  def addComponentToBredcrumb[B<:Breadcrumb, C<:Component]
  (breadcrumb: B, component: C)
  (implicit unionO: UnionHelper[B#Shape#OutputShape, C#Shape#InputShape], unionI: UnionHelper[B#Shape#InputShape, C#Shape#OutputShape]): Breadcrumb {
    type Shape = BreadcrumbShape {
      type OutputShape = unionO.Out
      type InputShape = unionI.Out
    }
  } = ???
}
