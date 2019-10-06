package ru.primetalk.contacts.core

import scala.annotation.implicitNotFound
import scala.language.reflectiveCalls

sealed trait TypeSets1 {
  //
  sealed trait TypeSet extends Serializable with Product {
    private[core] def elements: Set[Any]
    override def equals(o: Any): Boolean = o match {
      case t: TypeSet => elements == t.elements
      case _ => false
    }
    override def hashCode(): Int = elements.hashCode()
  }
  case object Empty extends TypeSet {
    private[core] def elements: Set[Any] = Set.empty
  }
  type Empty = Empty.type
  // ∅ - \u2205 - synonyms
  type ∅ = Empty
  val ∅ : Empty.type = Empty
  // This class enumerates elements of the set.
  // In order to avoid duplicates, we make constructor private and
  // take special measures for deduplication.
  // Deduplication trick:
  // The trick with the Scala implicit priorities.
  // In this trait TypeSets2 we define general case, and in
  // the next trait TypeSets1 we define the case when element belongs to the set.
  final case class ConsTypeSet[E, S <: TypeSet] private (e: E,s: S) extends TypeSet {
    private[core] lazy val elements: Set[Any] = s.elements.+(e)
    override def equals(o: Any): Boolean = o match {
      case t: TypeSet => elements == t.elements
      case _ => false
    }
    override def hashCode(): Int = elements.hashCode()
  }
  // This is an operator for representing sets without duplications
  type +:[E, S <: TypeSet] = ConsTypeSet[E, S]
  type Singleton[E] = E +: ∅
  // ⊕ - \u2295
  sealed trait AddElement[E, S<:TypeSet] {
    type Sum <: TypeSet
    def apply(e: E, s: S): Sum
    def unwrap(sum: Sum): (E, S)
  }

  def addElement[E, S<:TypeSet](e: E, s: S)(implicit addWrapper: AddElement[E,S]): addWrapper.Sum =
    addWrapper.apply(e,s)

  trait AddElementTypeSetOps[S<:TypeSet] {
    def s: S
    def +:[E](e: E)(implicit addWrapper: AddElement[E,S]): addWrapper.Sum =
      addWrapper.apply(e,s)
  }
  implicit def getAddElementPriority2[E, S<:TypeSet]: AddElement[E, S] {
    type Sum = ConsTypeSet[E, S]
  } = new AddElement[E,S] {
    override type Sum = E ConsTypeSet S
    override def apply(e: E, s: S): Sum = ConsTypeSet[E,S](e,s)
    override def unwrap(sum: Sum): (E, S) = (sum.e, sum.s)
  }
//
//  @implicitNotFound("Couldn't add element to set")
//  sealed trait AddElementHelper[E, S<:TypeSet] {
//    type Out <: TypeSet
//    //def out(e: E, s: S): Out
//    //def unapply(es: Out): (E, S)
//  }
//
//  object AddElementHelper {
//    implicit def AddElementHelperAddElementToTypeSet[E, S<:TypeSet]: AddElementHelper[E, S] {
//      type Out = E ConsTypeSet S
//    } =
//      new AddElementHelper[E,S] {
//        type Out = E ConsTypeSet S
//        // def out(e: E, s: S): Out = ConsSet(e, s)
//        // def unapply(es: Out): (E, S) = (es.element, es.set)
//      }
//  }


  @implicitNotFound("Couldn't prove that each element of TypeSet is subtype the given Up type")
  sealed trait EachElementIsSubtype[Up, TypeSet]
  object EachElementIsSubtype {
    implicit def Empty[Up]: EachElementIsSubtype[Up, Empty] =
      new EachElementIsSubtype[Up, Empty] {}
    implicit def Cons[Up, E <: Up, S <: TypeSet](implicit ev: EachElementIsSubtype[Up, S]): EachElementIsSubtype[Up, ConsTypeSet[E, S]] =
      new EachElementIsSubtype[Up, ConsTypeSet[E, S]] {}
  }

  @implicitNotFound("Couldn't prove that element belongs to set")
  sealed trait BelongsTo[Element, S <: TypeSet] {
    def extract(s: S): Element
  }
  // ∊ - \u220A
  type ∊[Element, S <: TypeSet] = BelongsTo[Element, S]
  object BelongsTo {
    implicit def elementIsHeadOfTypeSet0[E, S <: TypeSet]: E ∊ (E ConsTypeSet S) =
      new BelongsTo[E, E ConsTypeSet S] {
        def extract(s: E ConsTypeSet S): E = s.e
      }
    implicit def elementBelongsToTailOfTypeSet0[E, H, S <: TypeSet](implicit b: E ∊ S): E ∊ (H ConsTypeSet S) =
      new BelongsTo[E, H ConsTypeSet S] {
        def extract(s: H ConsTypeSet S): E = b.extract(s.s)
      }
  }
  // ∀ - \u2200
  @implicitNotFound("Couldn't prove that predicate holds true for each element")
  sealed trait ForAll[P[_], S<: TypeSet]
  object ForAll {
    implicit def empty[P[_]]: ForAll[P, Empty] = new ForAll[P, Empty] {}
    implicit def cons[P[_], E, S<: TypeSet](implicit p: P[E], forAll: ForAll[P, S]): ForAll[P, E +: S] = new ForAll[P, E +: S] {}
  }

  type ∀[P[_], S <: TypeSet] = ForAll[P, S]

  @implicitNotFound("Couldn't prove that each element of TypeSet is subtype the given Up type")
  sealed trait IsSubtype[Up, T]
  implicit def isSubtype[Up, T <: Up]: IsSubtype[Up, T] = new IsSubtype[Up, T] {}
  type EachElementIsSubtype2[Up, S<: TypeSet] = ForAll[({ type lambda[T] = IsSubtype[Up, T]})#lambda, S]

  // ∃ - \u2203
  @implicitNotFound("Couldn't prove that predicate holds true for each element")
  sealed trait Exists[P[_], S<: TypeSet]

  implicit def consTail[P[_], E, S<: TypeSet](implicit exists: Exists[P, S]): Exists[P, E +: S] = new Exists[P, E +: S] {}

  type ∃[P[_], S <: TypeSet] = Exists[P, S]

  @implicitNotFound("Couldn't prove that typesets are equal")
  trait TypeSetEq[A<:TypeSet, B<:TypeSet]
  object TypeSetEq {
    implicit def typeSetEq[A<:TypeSet, B<:TypeSet](implicit ev1: IsSubset[A,B], ev2: IsSubset[B,A]): TypeSetEq[A, B] = new TypeSetEq[A, B]{}
  }
  /**
    * IsSubset type-level operation.
    * O(N^^2)
    * TODO: implement O(N) hash-based implementation.
    */
  @implicitNotFound("Couldn't prove that set is in another set")
  sealed trait IsSubset[Subset <: TypeSet, SuperSet <: TypeSet]
  // ⊂ - \u2282
  type ⊂[Subset <: TypeSet, SuperSet <: TypeSet] = IsSubset[Subset, SuperSet]

  object IsSubset {
    implicit def empty[SuperSet2<:TypeSet]:
      IsSubset[∅, SuperSet2] = new IsSubset[∅, SuperSet2] {}
    implicit def cons[E, S <: TypeSet, SuperSet<:TypeSet](implicit headBelongs: E ∊ SuperSet, tailIsSubset: S ⊂ SuperSet):
      IsSubset[E ConsTypeSet S, SuperSet] = new IsSubset[E ConsTypeSet S, SuperSet]{}
  }

  type IsSubset2[Subset <: TypeSet, SuperSet <: TypeSet] = ForAll[({type P[E] = BelongsTo[E, SuperSet]})#P, Subset]
  //type IsSubset3[Subset <: TypeSet, SuperSet <: TypeSet] = ForAll[BelongsTo[?, SuperSet], Subset]

  @implicitNotFound("Couldn't find all elements as implicits")
  sealed trait RenderTypeSet[S <: TypeSet] {
    def apply: S
  }
  implicit def RenderTypeSetEmpty: RenderTypeSet[Empty] = new RenderTypeSet[Empty] {
    def apply: Empty = Empty
  }
  implicit def RenderTypeSetCons[E, S <: TypeSet](implicit renderTypeSet: RenderTypeSet[S], e: ValueOf[E]): RenderTypeSet[E ConsTypeSet S] = new RenderTypeSet[E ConsTypeSet S]{
    override def apply: ConsTypeSet[E, S] = ConsTypeSet(e.value, renderTypeSet.apply)
  }
}
sealed trait TypeSets0 extends TypeSets1 {
  implicit def consHead[P[_], E, S<: TypeSet](implicit p: P[E]): Exists[P, E +: S] = new Exists[P, E +: S] {}

  implicit def getAddElementPriority1[E, S<:TypeSet](implicit ev: E ∊ S ): AddElement[E, S] {
    type Sum = S
  } = new AddElement[E,S] {
    override type Sum = S
    def apply(e: E, s: S): Sum = s
    override def unwrap(sum: Sum): (E, S) = (ev.extract(sum), sum)
  }


  sealed trait UnionHelper[A <: TypeSet, B <: TypeSet] {
    type Out <: TypeSet
    def apply(a: A, b: B): Out
    def unwrap(o: Out): (A, B)
  }
  implicit def UnionHelperEmpty[B <: TypeSet]: UnionHelper[∅, B] =
    new UnionHelper[∅, B] {
      type Out = B
      def apply(a: ∅, b: B): Out = b
      def unwrap(o: Out): (∅, B) = (∅, o)
    }
  // this method will be used if `E` doesn't belong to `B`
  implicit def UnionHelperConsPriority0[E, S<: TypeSet, B <: TypeSet](implicit unionSB: S UnionHelper B): UnionHelper[E ConsTypeSet S, B] =
    new UnionHelper[E ConsTypeSet S, B] {
      override type Out = E ConsTypeSet unionSB.Out
      def apply(ePlusS: E ConsTypeSet S, b: B): Out = {
        ConsTypeSet(ePlusS.e, unionSB.apply(ePlusS.s, b))
      }
      def unwrap(esb: E ConsTypeSet unionSB.Out): (E ConsTypeSet S, B) = {
        val (s,b) = unionSB.unwrap(esb.s)
        (ConsTypeSet(esb.e, s), b)
      }
    }
  sealed trait IntersectionHelper[A <: TypeSet, B <: TypeSet] {
    type Out <: TypeSet
    def apply(a: A, b: B): Out
    def unwrap(o: Out): (A, B)
  }

  implicit def IntersectionHelperEmpty[B <: TypeSet](implicit b: RenderTypeSet[B]): IntersectionHelper[∅, B] =
    new IntersectionHelper[∅, B] {
      type Out = ∅
      def apply(a: ∅, b: B): Out = a
      def unwrap(o: Out): (∅, B) = (∅, b.apply)
    }
  implicit def IntersectionHelperConsNotContains[E, S <: TypeSet, B <: TypeSet](implicit intersectSB: S IntersectionHelper B, e: ValueOf[E]): IntersectionHelper[E ConsTypeSet S, B] =
    new IntersectionHelper[E ConsTypeSet S, B] {
      type Out = intersectSB.Out
      def apply(a: E ConsTypeSet S, b: B): Out = intersectSB.apply(a.s, b)
      def unwrap(o: Out): (E ConsTypeSet S, B) = {
        val (s, b) = intersectSB.unwrap(o)
        (ConsTypeSet(e.value, s), b)
      }
    }
}

trait UnionTypeSets extends TypeSets0 {

  implicit def UnionHelperConsEInB[E, S<: TypeSet, B <: TypeSet](implicit unionSB: S UnionHelper B, eInB: E BelongsTo B): UnionHelper[E ConsTypeSet S, B] =
    new UnionHelper[E ConsTypeSet S, B] {
      override type Out = unionSB.Out
      def apply(ePlusS: E ConsTypeSet S, b: B): Out = {
        unionSB.apply(ePlusS.s, b)
      }
      def unwrap(esb: unionSB.Out): (E ConsTypeSet S, B) = {
        val (s,b) = unionSB.unwrap(esb)
        (ConsTypeSet(eInB.extract(b), s), b)
      }
    }

  trait UnionTypeSetOps[S<:TypeSet] {
    def s: S
    def ∪[B <: TypeSet](b: B)(implicit unionHelper: UnionHelper[S,B]): unionHelper.Out = unionHelper.apply(s, b)
  }
  // ∪ \u222A
  def ∪[A <: TypeSet, B <: TypeSet](a: A, b: B)(implicit unionHelper: UnionHelper[A,B]): unionHelper.Out =
    unionHelper(a,b)

}

trait IntersectTypeSets extends UnionTypeSets {
  implicit def IntersectionHelperConsContains[E, S <: TypeSet, B <: TypeSet](implicit intersectSB: S IntersectionHelper B, ev: E BelongsTo B): IntersectionHelper[E ConsTypeSet S, B] =
    new IntersectionHelper[E ConsTypeSet S, B] {
      type Out = E ConsTypeSet intersectSB.Out
      def apply(a: E ConsTypeSet S, b: B): Out = ConsTypeSet(a.e, intersectSB.apply(a.s, b))
      def unwrap(o: Out): (E ConsTypeSet S, B) = {
        val (s, b) = intersectSB.unwrap(o.s)
        (ConsTypeSet(o.e, s), b)
      }
    }

  trait IntersectionTypeSetOps[S<:TypeSet] {
    def s: S
    def ∩[B <: TypeSet](b: B)(implicit helper: IntersectionHelper[S,B]): helper.Out = helper.apply(s, b)
  }
  // ∩ \u2229
  def ∩[A <: TypeSet, B <: TypeSet](a: A, b: B)(implicit helper: IntersectionHelper[A,B]): helper.Out = helper.apply(a, b)


}

trait TypeSets extends IntersectTypeSets {

  implicit class TypeSetOps[S<:TypeSet](val s: S) extends
    AddElementTypeSetOps[S]
    with UnionTypeSetOps[S]
    with IntersectionTypeSetOps[S]
  {
    // runtime contains, O(N)
    def contains0[E](e: E): Boolean = s match {
      case ConsTypeSet(h, t) => h == e || t.contains0(e)
      case _ => false
    }
    def shouldContain[E](e: E)(implicit ev: E BelongsTo S): Unit = ()
    // compile-time contains, O(1), could be inlined
    def contains[E](e: E)(implicit ev: E BelongsTo S = null): Boolean = ev != null
  }
}
