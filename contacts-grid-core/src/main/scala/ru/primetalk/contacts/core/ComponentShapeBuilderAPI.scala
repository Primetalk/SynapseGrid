package ru.primetalk.contacts.core

import scala.language.reflectiveCalls
import UniSets._

import scala.annotation.tailrec

trait ComponentShapeBuilderAPI extends Signals {


//  case class ComponentShape2[InputShape <: UniSet, OutputShape <: UniSet](inputs: InputShape, outputs: OutputShape)


  sealed trait ComponentShape { self =>
    type InputShape <: UniSet
    type OutputShape <: UniSet
    // TODO: Set should depend on the types of input/output shapes
    //    val inputs: Render[Contact, InputShape]
    //    val outputs: Render[Contact, OutputShape]
    val inputs: Set[Contact]
    val outputs: Set[Contact]
  }


  object ComponentShape {
    def apply[I <: UniSet, O <: UniSet](implicit i: Render[Contact, I], o: Render[Contact, O])
    : ComponentShape {type InputShape = I; type OutputShape = O} =
      new ComponentShape {
        type InputShape = I
        type OutputShape = O
        val inputs: Set[Contact] = i.elements
        val outputs: Set[Contact] = o.elements
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
    // TODO: rename to `apply`
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
    override val handler: CompShape#InputShape >> CompShape#OutputShape = f
  }

//  def parallelAddComponentHandler[A, B, C](implicit h1: HandlerOf[...], h2:): Handler[C]
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
      val res =
        out1.map(_.cProjection[Union[Shape1#OutputShape, Shape2#OutputShape]]) ++
        out2.map(_.cProjection[Union[Shape1#OutputShape, Shape2#OutputShape]])
      res
    }
  }

  // set of contacts that belong to a system.
  // SourceShape - are contacts of inner subsystems that can emit signals
  // SinkShape - are contacts of inner subsystems that can consume signals.
  // TODO: Get rid of BreadboardShape. Just use ImplementationShape
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

//    def toComponent[I <: UniSet, O <: UniSet](inputs: I, outputs: O/*, limit: Int = 1000 */)(implicit inputs1: Render[Contact, I], outputs1: Render[Contact, O], i: I IsSubSetOf Shape#SinkShape, o: O IsSubSetOf Shape#SourceShape)
//    : Component[ComponentShape {type InputShape = I; type OutputShape = O}]


    //    override def toComponent[I <: UniSet, O <: UniSet]
    //    (implicit inputs1: Render[Contact, I], outputs1: Render[Contact, O],
    //     i: UniSets.IsSubSetOf[I, Union[BS#SinkShape, CS#InputShape]],
    //     o: UniSets.IsSubSetOf[O, Union[BS#SourceShape, CS#OutputShape]])
    //    : Component[ComponentShape {type InputShape = I;type OutputShape = O}]
    def toComponent[I <: UniSet, O <: UniSet]
    (implicit
     inputs1: Render[Contact, I],
     outputs1: Render[Contact, O],
     i: IsSubSetOf[I, Shape#SinkShape], o: IsSubSetOf[O, Shape#SourceShape],
     //nonOutputsIsSubsetOfInputs: IsSubSetOf[Subtract[Shape#SourceShape, O], Shape#SinkShape]// ! Important protection from losing data.
     nonOutputsIsSubsetOfInputs: IsSubSetOf[Shape#SourceShape, Union[Shape#SinkShape, O]]
    )
    : Component[ComponentShape {type InputShape = I; type OutputShape = O}] = new Component[ComponentShape {type InputShape = I;type OutputShape = O}] {
      override val shape: ComponentShape {type InputShape = I;type OutputShape = O} = new ComponentShape {
        override type InputShape = I
        override type OutputShape = O
        override val inputs: Set[Contact] = inputs1.elements
        override val outputs: Set[Contact] = outputs1.elements
      }
      override val handler: I >> O = signal => {
        @tailrec
        def loop(innerInputSignals: Iterable[Signal[Shape#SinkShape]], tempOutput: Iterable[Signal[O]]): Iterable[Signal[O]] = {
          if(innerInputSignals.isEmpty)
            tempOutput
          else {
            val innerResults = innerInputSignals.flatMap(tick)
            val renderer = implicitly[Render[Contact, O]]
            val sortedResults = innerResults.map{s => projection0EitherUnion[Shape#SinkShape, O, Shape#SourceShape](s)}
            val lefts = sortedResults.flatMap(_.left.toOption)
            val rights = sortedResults.flatMap(_.toOption)
            val leftsAsInputs = lefts.map(_.cProjection[Shape#SinkShape])
            loop(leftsAsInputs, tempOutput ++ rights)
          }
        }
        val inputSignal = signal.cProjection[Shape#SinkShape](i)
        loop(Iterable.single(inputSignal), Iterable.empty)
      }
    }

  }

  def addComponentToBreadboard[BS<:BreadboardShape, CS<:ComponentShape]
  (breadboard: Breadboard[BS], component: Component[CS])
  (implicit
   sinkContacts: Render[Contact, BS#SinkShape],
   sourceContacts: Render[Contact, BS#SourceShape],
   inputContacts: Render[Contact, CS#InputShape],
   outputContacts: Render[Contact, CS#OutputShape]
  )
    : Breadboard[BreadboardShape {
      type SourceShape = Union[BS#SourceShape, CS#OutputShape]
      type SinkShape = Union[BS#SinkShape, CS#InputShape]
    }] = new Breadboard[BreadboardShape {
    type SourceShape = Union[BS#SourceShape, CS#OutputShape]
    type SinkShape = Union[BS#SinkShape, CS#InputShape]
  }] { newBreadboard =>

    override val shape: BreadboardShape {
      type SourceShape = Union[BS#SourceShape, CS#OutputShape]
      type SinkShape = Union[BS#SinkShape, CS#InputShape]
    } = new BreadboardShape{
      type SourceShape = Union[BS#SourceShape, CS#OutputShape]
      type SinkShape = Union[BS#SinkShape, CS#InputShape]
      override val sources: Set[Contact] = breadboard.shape.sources ++ component.shape.outputs
      override val sinks: Set[Contact] = breadboard.shape.sinks ++ component.shape.inputs
    }

    override def implementation: Implementation = parallelAddComponent(breadboard.implementation, component)

  }

  def emptyBreadboard: Breadboard[EmptyBreadboardShape.type] = new Breadboard[EmptyBreadboardShape.type] {
    override val shape: EmptyBreadboardShape.type = EmptyBreadboardShape

    override def implementation: Implementation = new Component[ImplementationShape[EmptyBreadboardShape.type]] {
      override val shape: ImplementationShape[EmptyBreadboardShape.type] = EmptyComponentShape
      override val handler: Empty >> Empty =
        in => throw new IllegalArgumentException("emptyBreadboard.implementation.handler got signal: " + in)
    }
  }

}
