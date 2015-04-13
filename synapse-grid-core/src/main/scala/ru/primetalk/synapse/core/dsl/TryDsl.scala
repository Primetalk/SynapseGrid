package ru.primetalk.synapse.core.dsl

import scala.util.Try

/** DSL for working with links that catch exceptions and encapsulate them in Try monad.*/
trait TryDsl extends SystemBuilderApi with NextLabelExt with SystemBuilderDslApi {
  implicit class TryLinkBuilderOps[T1, T2](c: (Contact[T1], Contact[Try[T2]]))(implicit sb: SystemBuilder) {

    /** If map is used with a try-contact, then it will automatically encapsulate
      * function into Try. */
    def tryMap(f: T1 ⇒ T2, name: String = ""): Contact[Try[T2]] =
      sb.addLink(c._1, c._2, sb.nextLabel(name, "Try{" + f + "}"),
        new FlatMapLink[T1, Try[T2]](x => Seq(Try {
          f(x)
        })))
  }

  implicit class TryContactOps[T](val c: Contact[Try[T]])(implicit sb: SystemBuilder) {
    /** Extracts an exception from Try. It only produces a signal when there was an exception. */
    def recover: Contact[Throwable] =
      new ContactOps[Try[T]](c)(sb).flatMap(t => if (t.isSuccess) Seq() else Seq(t.failed.get), "recover")

    /** pass data further if there were no exception. Unwraps Try monad. */
    def success: Contact[T] =
      new ContactOps[Try[T]](c)(sb).flatMap(t => if (t.isSuccess) Seq(t.get) else Seq(), "success")
  }

  implicit class TryFlatMapContactOps[T](val c: Contact[Try[TraversableOnce[T]]])(implicit sb: SystemBuilder) {
    /** Flatterns the output of a tryMap. If there was an exception, an empty list is returned */
    def flatten: Contact[T] =
      new ContactOps[Try[TraversableOnce[T]]](c)(sb).flatMap(t => if (t.isSuccess) t.get else Seq(), "flatten")
  }

  /** New methods available on contacts that construct links.
    */
  implicit class ContactTryOps[T](val c: Contact[T])(implicit sb: SystemBuilder) {
    require(c != null, "Contact is null. " +
      "This can usually happen when the contact is declared using val, " +
      "but it is placed further down the source code and thus has not been initialized yet.")
    /** Creates another contact and links it to this one with transformation f.
      * Exceptions are caught and encapsulated in Try.*/
    def tryMap[T2](f: T ⇒ T2, name: String = ""): Contact[Try[T2]] =
      c.map((t: T) => Try(f(t)), sb.nextLabel(name, "tryMap(" + f + ")"))

  }

}
