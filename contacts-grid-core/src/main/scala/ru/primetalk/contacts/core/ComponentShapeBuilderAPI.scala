package ru.primetalk.contacts.core

import scala.language.reflectiveCalls
import TypeSets._
import ru.primetalk.contacts.core

trait ComponentShapeBuilderAPI extends Signals {


//  case class ComponentShape2[InputShape <: TypeSet, OutputShape <: TypeSet](inputs: InputShape, outputs: OutputShape)


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

  // concatenates components so that they have concatenated inputs, outputs and handlers.
  def parallelAddComponent[//Shape1 <: ComponentShape, Shape2 <: ComponentShape,
    Comp1 <: Component,//{ type Shape = Shape1 },
    Comp2 <: Component//{ type Shape = Shape2 }
  ](comp1: Comp1, comp2: Comp2)(
   implicit
   inputShapesUnion: UnionHelper[comp1.Shape#InputShape, comp2.Shape#InputShape],
   outputShapesUnion: UnionHelper[comp1.Shape#OutputShape, comp2.Shape#OutputShape],
   signalOnContactsOps: SignalOnContactsOps[SignalOnContacts]
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
      val (s1, s2) = (new unwrapSignal2(comp1.shape.inputs, comp2.shape.inputs, inputShapesUnion)).apply(signalOnContacts)
      val out1 = s1.flatMap(a => comp1.handler(a))
      val out2 = s2.flatMap(a => comp2.handler(a))
      val res =  out1.flatMap(s => signalOnContactsOps.projection0
        [comp1.Shape#OutputShape, outputShapesUnion.Out, SignalOnContacts[comp1.Shape#OutputShape]](s, shape.outputs)
      ) ++
        out2.flatMap(s => signalOnContactsOps.projection0
          [comp2.Shape#OutputShape, outputShapesUnion.Out, SignalOnContacts[comp2.Shape#OutputShape]](s, shape.outputs)
      )
      res
    }
  }

  // set of contacts that belong to a system.
  // Outputs - are contacts of inner subsystems that can emit signals
  // Inputs - are contacts of inner subsystems that can consume signals.
  sealed trait BreadboardShape { self =>
    type SourceShape <: TypeSet
    type SinkShape <: TypeSet
    val sources: SourceShape
    val sinks: SinkShape
    type ImplementationShape = ComponentShape {
      type InputShape = self.SinkShape
      type OutputShape = self.SourceShape
    }
  }
  case object EmptyBreadboardShape extends BreadboardShape {
    type SourceShape = Empty
    type SinkShape = Empty
    val sources: SourceShape = Empty
    val sinks: SinkShape = Empty
  }
  // In Out

  trait Breadboard { breadboard =>
    type Shape <: BreadboardShape
    def shape: Shape
    def projectSignals: SignalOnContacts[Shape#SourceShape] => Iterable[SignalOnContacts[Shape#SinkShape]]
    type Implementation <: Component {
      type Shape = ComponentShape {
        type InputShape = breadboard.Shape#SinkShape
        type OutputShape = breadboard.Shape#SourceShape
      }
    }
    def implementation: Implementation
    def tick: SignalOnContacts[Shape#SinkShape] => Iterable[SignalOnContacts[Shape#SourceShape]] = implementation.handler
    def toComponent[I <: TypeSet, O <: TypeSet](inputs: I, outputs: O)(implicit i: I IsSubsetOf Shape#SinkShape, o: O IsSubsetOf Shape#SourceShape): Component {
      type Shape = ComponentShape {
        type InputShape = I
        type OutputShape = O
      }
    }
  }

  def addComponentToBreadboard[B<:Breadboard, C<:Component]
  (breadboard: B, component: C)
  (implicit
   bbUnionSinks: UnionHelper[breadboard.Shape#SinkShape, component.shape.InputShape],
   bbUnionSources: UnionHelper[breadboard.Shape#SourceShape, component.shape.OutputShape],
   implUnionII: UnionHelper[breadboard.Implementation#Shape#InputShape, component.Shape#InputShape],
   implUnionOO: UnionHelper[breadboard.Implementation#Shape#OutputShape, component.Shape#OutputShape],
   signalOnContactsOps: SignalOnContactsOps[SignalOnContacts]
  ): Breadboard {
    type Shape = BreadboardShape {
      type SourceShape = bbUnionSources.Out
      type SinkShape = bbUnionSinks.Out
    }
    type Implementation = Component {
      type Shape = ComponentShape {
        type InputShape = implUnionII.Out
        type OutputShape = implUnionOO.Out
      }
    }
  } = new Breadboard {
    type Shape = BreadboardShape {
      type SourceShape = bbUnionSources.Out
      type SinkShape = bbUnionSinks.Out
    }
    type Implementation = Component {
      type Shape = ComponentShape {
        type InputShape = implUnionII.Out
        type OutputShape = implUnionOO.Out
      }
    }
    override def shape: Shape = new BreadboardShape {
      type SourceShape = bbUnionSources.Out
      type SinkShape = bbUnionSinks.Out
      override val sources: bbUnionSources.Out = bbUnionSources.apply(breadboard.shape.sources, component.shape.outputs)
      override val sinks: bbUnionSinks.Out = bbUnionSinks.apply(breadboard.shape.sinks, component.shape.inputs)
    }
    override def projectSignals: SignalOnContacts[bbUnionSources.Out] => Iterable[SignalOnContacts[bbUnionSinks.Out]] = s =>
      signalOnContactsOps.projection0[bbUnionSources.Out, bbUnionSinks.Out, SignalOnContacts[bbUnionSources.Out]](s, shape.sinks)

    override def implementation: Implementation =
      parallelAddComponent[breadboard.Implementation, component.type ](
        breadboard.implementation, component)(
        implUnionII, implUnionOO, signalOnContactsOps)

    override def toComponent[I <: core.TypeSets.TypeSet, O <: core.TypeSets.TypeSet](inputs1: I, outputs1: O)(implicit i: core.TypeSets.IsSubsetOf[I, bbUnionSinks.Out], o: core.TypeSets.IsSubsetOf[O, bbUnionSources.Out]): Component {
      type Shape = ComponentShape {
        type InputShape = I
        type OutputShape = O
      }
    } = new Component {
      override type Shape = ComponentShape {
        type InputShape = I
        type OutputShape = O
      }
      override val shape: Shape = new ComponentShape {
        override type InputShape = I
        override type OutputShape = O
        override val inputs: InputShape = inputs1
        override val outputs: OutputShape = outputs1
      }
      override val handler: Handler = ???
    }
  }

}
