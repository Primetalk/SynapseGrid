package ru.primetalk.contacts.dot

trait Show[T] {
  def show(t: T): String
}

object Show {
  implicit class ShowOps[T: Show](t: T) {
    def show: String = implicitly[Show[T]].show(t)
  }
}
