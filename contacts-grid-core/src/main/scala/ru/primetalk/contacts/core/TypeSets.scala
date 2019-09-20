package ru.primetalk.contacts.core


sealed trait TypeSets0 {
  sealed trait TypeSet
  case object Empty extends TypeSet
  // ∅ - \u2205
  type ∅ = Empty.type
  // This class enumerates elements of the set.
  // In order to avoid duplicates, we make constructor private and use
  // the trick with the implicit priorities.
  // In this trait we define general case, and in
  // the next trait we define the case when element belongs to the set.
  case class |+|[E, S <: TypeSet] private (element: E, set: S) extends TypeSet
  // ⊕ - \u2295
  type +[E, S<:TypeSet] = AddElementHelper[E, S]#Out
  trait AddElementHelper[E, S<:TypeSet] {
    type Out <: TypeSet
    def out(e: E, s: S): Out
  }
  implicit def AddElementHelperAddElementToTypeSet[E, S<:TypeSet]: AddElementHelper[E,S] =
    new AddElementHelper[E,S] {
      type Out = E |+| S
      def out(e: E, s: S): Out = |+|(e, s)
    }

  trait BelongsTo[Element, S <: TypeSet]
  // ∊ - \u220A
  type ∊[Element, S <: TypeSet] = BelongsTo[Element, S]
  object BelongsTo {
    implicit def elementBelongsToHeadOfTypeSet[E, S <: TypeSet]: E ∊ (E + S) =
      new BelongsTo[E, E + S] {}
    implicit def elementBelongsToTailOfTypeSet[E, H, S<: TypeSet](implicit b: E ∊ S): E ∊ (H + S) =
      new BelongsTo[E, H + S] {}
  }
}

trait TypeSets extends TypeSets0 {

  implicit def AddElementHelperAddElementToTypeSetWhenEBelongsToS[E, S<:TypeSet](implicit ev: E ∊ S): AddElementHelper[E,S] =
    new AddElementHelper[E,S] {
      type Out = S
      def out(e: E, s: S): Out = s
    }

  // ∪ \u222A
  type ∪[A <: TypeSet, B <: TypeSet] = UnionHelper[A, B]#Out

  sealed trait UnionHelper[A <: TypeSet, B <: TypeSet] {
    type Out <: TypeSet
  }

  object UnionHelper {
    implicit def caseAIsEmpty[B <: TypeSet]: UnionHelper[∅, B] =
      new UnionHelper[∅, B] {type Out = B}
    implicit def caseAHeadTail[E, S<: TypeSet, B <: TypeSet](implicit ev: UnionHelper[S,B]): UnionHelper[E + S, B] =
      new UnionHelper[E + S, B] {type Out = E + ev.Out}
  }
}
