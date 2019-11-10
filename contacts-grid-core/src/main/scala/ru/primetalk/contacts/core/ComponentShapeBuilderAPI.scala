package ru.primetalk.contacts.core

import scala.language.reflectiveCalls
import UniSets._
import ru.primetalk.contacts.core

trait ComponentShapeBuilderAPI extends Signals {


//  case class ComponentShape2[InputShape <: UniSet, OutputShape <: UniSet](inputs: InputShape, outputs: OutputShape)


  sealed trait ComponentShape {
    type InputShape <: UniSet
    type OutputShape <: UniSet
    val inputs: Set[Contact]
    val outputs: Set[Contact]
  }

  object EmptyComponentShape extends ComponentShape {
    override type InputShape = ∅
    override type OutputShape = ∅

    override val inputs: Set[Contact] = Set()

    override val outputs: Set[Contact] = Set()
  }

  def addInput[C<:Contact, Comp0 <: ComponentShape](c: C, componentShape: Comp0)(
      implicit InputShapeAddC: Render[Contact, Insert[C, Comp0#InputShape]]
    ): ComponentShape{
      type InputShape = Insert[C, Comp0#InputShape]
      type OutputShape = Comp0#OutputShape
    } = new ComponentShape {
      override type InputShape = Insert[C, Comp0#InputShape]
      override type OutputShape = Comp0#OutputShape

      override val inputs: Set[Contact] = InputShapeAddC.elements

      override val outputs: Set[Contact] = componentShape.outputs
    }

  def addOutput[C<:Contact, Comp0 <: ComponentShape](c: C, componentShape: Comp0)(
    implicit OutputShapeAddC: Render[Contact, Insert[C, Comp0#OutputShape]]
  ): ComponentShape{
    type InputShape = componentShape.InputShape
    type OutputShape = Insert[C, Comp0#OutputShape]
  } = new ComponentShape {
    type InputShape = componentShape.InputShape
    type OutputShape = Insert[C, Comp0#OutputShape]

    override val inputs: Set[Contact] = componentShape.inputs

    override val outputs: Set[Contact] = OutputShapeAddC.elements
  }

  def componentShapeConverter[A<:UniSet,B,C,D](shape1)(): shape2  = shape1.asInstanceOf[]
  def InOutShape[In<:Contact:ValueOf, Out<:Contact:ValueOf](in: In, out: Out)(
  ): ComponentShape{
    type InputShape = Singleton[In]
    type OutputShape =  Singleton[Out]
  } = addOutput(out, addInput(in, EmptyComponentShape))
//
//  sealed trait Component {
//    type Shape <: ComponentShape
//    type Handler = SignalOnContacts[shape.InputShape] => Iterable[SignalOnContacts[shape.OutputShape]]
//    val shape: Shape
//    val handler: Handler
//  }
//
//  type ShapedComponent[S] = Component { type Shape = S}
//
//  def createComponent[CompShape <: ComponentShape]
//  (shape0: CompShape)
//  (f: SignalOnContacts[shape0.InputShape] => Iterable[SignalOnContacts[shape0.OutputShape]])
////  (
////    implicit
////    evInputContacts: EachElementIsSubtype[Contact, InputContacts],
////    evInputs: InputContacts ⊂ CompShape#InputShape,
////    evOutputs: OutputSignal#Contact ∊ CompShape#OutputShape
////  )
//  : Component {
//    type Shape = shape0.type
//    type Handler = SignalOnContacts[shape0.InputShape] => Iterable[SignalOnContacts[shape0.OutputShape]]
//  } = new Component {
//    override type Shape = shape0.type
//    override val shape = shape0
//    override type Handler = SignalOnContacts[shape0.InputShape] => Iterable[SignalOnContacts[shape0.OutputShape]]
//    override val handler: Handler = f
//  }

//
//  type Plus[Shape1 <: ComponentShape, Shape2 <: ComponentShape] = ComponentShape{
//    type InputShape = Shape1#InputShape ∪ Shape2#InputShape
//    type OutputShape = Shape1#OutputShape ∪ Shape2#OutputShape
//  }
//
//  // concatenates components so that they have concatenated inputs, outputs and handlers.
//  def parallelAddComponent[//Shape1 <: ComponentShape, Shape2 <: ComponentShape,
//    Comp1 <: Component,//{ type Shape = Shape1 },
//    Comp2 <: Component//{ type Shape = Shape2 }
//  ](comp1: Comp1, comp2: Comp2)(
//   implicit
//   inputShapesUnion: UnionHelper[comp1.shape.InputShape, comp2.shape.InputShape],
//   outputShapesUnion: UnionHelper[comp1.shape.OutputShape, comp2.shape.OutputShape],
//   signalOnContactsOps: SignalOnContactsOps[SignalOnContacts]
//  ): Component {
//    type In1 = inputShapesUnion.Out
//    type Out1 = outputShapesUnion.Out
//    type Shape = ComponentShape {
//      type InputShape = In1
//      type OutputShape = Out1
//    }
//  } = new Component {
//    type In1 = inputShapesUnion.Out
//    type Out1 = outputShapesUnion.Out
//    override type Shape = ComponentShape {
//      type InputShape = In1
//      type OutputShape = Out1
//    }
//    override val shape: Shape = new ComponentShape {
//      type InputShape = inputShapesUnion.Out
//      type OutputShape = outputShapesUnion.Out
//      override val inputs: InputShape = inputShapesUnion.apply(comp1.shape.inputs, comp2.shape.inputs)
//      override val outputs: OutputShape = outputShapesUnion.apply(comp1.shape.outputs, comp2.shape.outputs)
//    }
//    override type Handler = SignalOnContacts[inputShapesUnion.Out] => Iterable[SignalOnContacts[outputShapesUnion.Out]]
//    def castContactAny[A<:UniSet](s: SignalOnContacts[A]): SignalOnContacts[A]{type C = Contact} = s.asInstanceOf[SignalOnContacts[A]{type C = Contact}]
//    override val handler: Handler = signalOnContacts => {
//      val (s1, s2) = (new unwrapSignal2(comp1.shape.inputs, comp2.shape.inputs)(inputShapesUnion)).apply(signalOnContacts)
//      val out1: Iterable[SignalOnContacts[comp1.shape.OutputShape]] = s1.flatMap(a => comp1.handler(a))
//      val out2: Iterable[SignalOnContacts[comp2.shape.OutputShape]] = s2.flatMap(a => comp2.handler(a))
//      val res =
//       ??? // out1.flatMap(signalOnContactsOps.projection00Contact(shape.outputs)(_)) ++
//        // out2.flatMap(signalOnContactsOps.projection00Contact(shape.outputs)(_))
//      res
//    }
//  }
//
//  // set of contacts that belong to a system.
//  // Outputs - are contacts of inner subsystems that can emit signals
//  // Inputs - are contacts of inner subsystems that can consume signals.
//  sealed trait BreadboardShape { self =>
//    type SourceShape <: UniSet
//    type SinkShape <: UniSet
//    val sources: SourceShape
//    val sinks: SinkShape
//    type ImplementationShape = ComponentShape {
//      type InputShape = self.SinkShape
//      type OutputShape = self.SourceShape
//    }
//  }
//  case object EmptyBreadboardShape extends BreadboardShape {
//    type SourceShape = Empty
//    type SinkShape = Empty
//    val sources: SourceShape = Empty
//    val sinks: SinkShape = Empty
//  }
//  // In Out
//
//  trait Breadboard { breadboard =>
//    type Shape <: BreadboardShape
//    val shape: Shape
//    def projectSignals: SignalOnContacts[Shape#SourceShape] => Iterable[SignalOnContacts[Shape#SinkShape]]
//    type Implementation <: Component {
//      type Shape = ComponentShape {
//        type InputShape = breadboard.shape.SinkShape
//        type OutputShape = breadboard.shape.SourceShape
//      }
//    }
//    def implementation: Implementation
//    def tick: SignalOnContacts[Shape#SinkShape] => Iterable[SignalOnContacts[Shape#SourceShape]] = ??? //implementation.handler
//    def toComponent[I <: UniSet, O <: UniSet](inputs: I, outputs: O)(implicit i: I IsSubsetOf Shape#SinkShape, o: O IsSubsetOf Shape#SourceShape): Component {
//      type Shape = ComponentShape {
//        type InputShape = I
//        type OutputShape = O
//      }
//    }
//  }
//
//  def addComponentToBreadboard[B<:Breadboard, C<:Component]
//  (breadboard: B, component: C)
//  (implicit
//   bbUnionSinks: UnionHelper[breadboard.shape.SinkShape, component.shape.InputShape],
//   bbUnionSources: UnionHelper[breadboard.shape.SourceShape, component.shape.OutputShape],
//   implUnionII: UnionHelper[breadboard.Implementation#Shape#InputShape, component.Shape#InputShape],
//   implUnionOO: UnionHelper[breadboard.Implementation#Shape#OutputShape, component.Shape#OutputShape],
//   signalOnContactsOps: SignalOnContactsOps[SignalOnContacts]
//  ): Breadboard {
//    type Shape = BreadboardShape {
//      type SourceShape = bbUnionSources.Out
//      type SinkShape = bbUnionSinks.Out
//    }
//    type Implementation = Component {
//      type Shape = ComponentShape {
//        type InputShape = bbUnionSinks.Out
//        type OutputShape = bbUnionSources.Out
//      }
//    }
//  } = new Breadboard { newBreadboard =>
//    type Shape = BreadboardShape {
//      type SourceShape = bbUnionSources.Out
//      type SinkShape = bbUnionSinks.Out
//    }
//    type Implementation = Component {
//      type Shape = ComponentShape {
//        type InputShape = newBreadboard.shape.SinkShape
//        type OutputShape = newBreadboard.shape.SourceShape
//      }
//    }
//    override val shape: Shape = new BreadboardShape {
//      type SourceShape = bbUnionSources.Out
//      type SinkShape = bbUnionSinks.Out
//      override val sources: bbUnionSources.Out = bbUnionSources.apply(breadboard.shape.sources, component.shape.outputs)
//      override val sinks: bbUnionSinks.Out = bbUnionSinks.apply(breadboard.shape.sinks, component.shape.inputs)
//    }
//    override def projectSignals: SignalOnContacts[bbUnionSources.Out] => Iterable[SignalOnContacts[bbUnionSinks.Out]] = s =>
//    ??? //  signalOnContactsOps.projection0[bbUnionSources.Out, bbUnionSinks.Out, SignalOnContacts[bbUnionSources.Out]](s, shape.sinks)
//
//    override def implementation: Implementation = ???
////      parallelAddComponent[breadboard.Implementation, C](
////        breadboard.implementation, component)//(
////        bbUnionSinks, bbUnionSources, signalOnContactsOps)
//
//    override def toComponent[I <: core.UniSets.UniSet, O <: core.UniSets.UniSet](inputs1: I, outputs1: O)(implicit i: core.UniSets.IsSubsetOf[I, bbUnionSinks.Out], o: core.UniSets.IsSubsetOf[O, bbUnionSources.Out]): Component {
//      type Shape = ComponentShape {
//        type InputShape = I
//        type OutputShape = O
//      }
//    } = new Component {
//      override type Shape = ComponentShape {
//        type InputShape = I
//        type OutputShape = O
//      }
//      override val shape: Shape = new ComponentShape {
//        override type InputShape = I
//        override type OutputShape = O
//        override val inputs: InputShape = inputs1
//        override val outputs: OutputShape = outputs1
//      }
//      override val handler: Handler = signalOnContacts => {
////        val results = signalOnContactsOps.projection0(signalOnContacts, newBreadboard.shape.sinks).toIterable.flatMap{ signalOnInputs =>
////          tick.apply(signalOnInputs)
////        }
////        results.flatMap(s => signalOnContactsOps.projection0(s, outputs1))
//        ???
//      }
//    }
//  }

}
