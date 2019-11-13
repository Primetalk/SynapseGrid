package ru.primetalk.contacts.core

import scala.language.reflectiveCalls
import UniSets._

trait ComponentShapeBuilderAPI extends Signals {


//  case class ComponentShape2[InputShape <: UniSet, OutputShape <: UniSet](inputs: InputShape, outputs: OutputShape)


  sealed trait ComponentShape { self =>
    type InputShape <: UniSet
    type OutputShape <: UniSet
    val inputs: Set[Contact]
    val outputs: Set[Contact]

  }


  object ComponentShape {
    def apply[I <: UniSet, O <: UniSet](implicit i: Render[Contact, I], o: Render[Contact, O])
    : ComponentShape {type InputShape = I; type OutputShape = O} =
      new ComponentShape {
        type InputShape = I;
        type OutputShape = O
        val inputs = i.elements
        val outputs = o.elements
      }
    type Plus[Shape1 <: ComponentShape, Shape2 <: ComponentShape] = ComponentShape{
      type InputShape = Shape1#InputShape ∪ Shape2#InputShape
      type OutputShape = Shape1#OutputShape ∪ Shape2#OutputShape
    }

    def add[Shape1 <: ComponentShape, Shape2 <: ComponentShape](shape1: Shape1, shape2: Shape2)
    : Plus[Shape1, Shape2] =
      new ComponentShape {type InputShape = Union[Shape1#InputShape, Shape2#InputShape]; type OutputShape = Union[Shape1#OutputShape, Shape2#OutputShape]
        override val inputs: Set[Contact] = shape1.inputs ++ shape2.inputs
        override val outputs: Set[Contact] = shape1.outputs ++ shape2.outputs
      }

  }
  // we can only cast component shape because InputShape and OutputShape are ephemeral type-level-only sets.
  // If we ever try to materialize the sets, we will need a conversion.
  def componentShapeConverter[I1<:UniSet,O1<:UniSet,I2<:UniSet,O2<:UniSet]
  (shape1: ComponentShape{type InputShape = I1; type OutputShape = O1})
  (implicit ieq: Equal[I1, I2], oeq: Equal[O1, O2])
  : ComponentShape{type InputShape = I2; type OutputShape = O2} =
    shape1.asInstanceOf[ComponentShape{type InputShape = I2; type OutputShape = O2}]

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

  def InOutShape[In<:Contact:ValueOf, Out<:Contact:ValueOf](in: In, out: Out)(
  ): ComponentShape{
    type InputShape = Singleton[In]
    type OutputShape =  Singleton[Out]
  } = ComponentShape[Singleton[In], Singleton[Out]]

  sealed trait Component[Shape <: ComponentShape] {
    val shape: Shape
    val handler: Shape#InputShape >> Shape#OutputShape
  }
//
//  type ShapedComponent[S] = Component { type Shape = S}
//
  def createComponent[CompShape <: ComponentShape]
  (shape0: CompShape)
  (f: CompShape#InputShape >> CompShape#OutputShape)
  : Component[CompShape] = new Component[CompShape] {
    type Shape = CompShape
    override val shape: Shape = shape0
    override val handler = f
  }

  // concatenates components so that they have concatenated inputs, outputs and handlers.
  def parallelAddComponent[Shape1 <: ComponentShape, Shape2 <: ComponentShape](comp1: Component[Shape1], comp2: Component[Shape2])(
     implicit
     i1: Render[Contact, Shape1#InputShape],
     i2: Render[Contact, Shape2#InputShape],
     o: Render[Contact, Union[Shape1#OutputShape, Shape2#OutputShape]]
  ): Component[ComponentShape {
      type InputShape = Union[Shape1#InputShape, Shape2#InputShape]
      type OutputShape = Union[Shape1#OutputShape, Shape2#OutputShape]
    }]
   = new Component[ComponentShape {
      type InputShape = Union[Shape1#InputShape, Shape2#InputShape]
      type OutputShape = Union[Shape1#OutputShape, Shape2#OutputShape]
    }] {
    override val shape: ComponentShape {
      type InputShape = Union[Shape1#InputShape, Shape2#InputShape]
      type OutputShape = Union[Shape1#OutputShape, Shape2#OutputShape]
    } = ComponentShape.add(comp1.shape, comp2.shape)
    override val handler: Union[Shape1#InputShape, Shape2#InputShape] >> Union[Shape1#OutputShape, Shape2#OutputShape] = signal => {
      val s1 = signal.projection0[Shape1#InputShape].toIterable
      val s2 = signal.projection0[Shape2#InputShape].toIterable//      val (s1, s2) = (new unwrapSignal2(comp1.shape.inputs, comp2.shape.inputs)(inputShapesUnion)).apply(signalOnContacts)
      val out1: Iterable[Signal[Shape1#OutputShape]] = s1.flatMap(a => comp1.handler(a))
      val out2: Iterable[Signal[Shape2#OutputShape]] = s2.flatMap(a => comp2.handler(a))
      val res = out1.map(_.cProjection[Union[Shape1#OutputShape, Shape2#OutputShape]]) ++
        out2.map(_.cProjection[Union[Shape1#OutputShape, Shape2#OutputShape]])
      res
    }
  }

  // set of contacts that belong to a system.
  // SourceShape - are contacts of inner subsystems that can emit signals
  // SinkShape - are contacts of inner subsystems that can consume signals.
  sealed trait BreadboardShape {
    type SourceShape <: UniSet
    type SinkShape <: UniSet
    val sources: Set[Contact]
    val sinks: Set[Contact]
  }
  type ImplementationShape[B<: BreadboardShape] = ComponentShape {
    type InputShape = B#SinkShape
    type OutputShape = B#SourceShape
  }
  case object EmptyBreadboardShape extends BreadboardShape {
    type SourceShape = Empty
    type SinkShape = Empty
    val sources: Set[Contact] = Set()
    val sinks: Set[Contact] = Set()
  }
  // In Out

  trait Breadboard[Shape <: BreadboardShape] { breadboard =>
    val shape: Shape
//    def projectSignals: SignalOnContacts[Shape#SourceShape] => Iterable[SignalOnContacts[Shape#SinkShape]]
    type Implementation = Component[ImplementationShape[Shape]]
    def implementation: Implementation
    def tick: Shape#SinkShape >> Shape#SourceShape = implementation.handler
    def toComponent[I <: UniSet, O <: UniSet](inputs: I, outputs: O)(implicit i: I IsSubSetOf Shape#SinkShape, o: O IsSubSetOf Shape#SourceShape)
    : Component[ComponentShape {
        type InputShape = I
        type OutputShape = O
      }]
  }
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
