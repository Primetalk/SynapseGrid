package ru.primetalk.contacts.core

trait ContactsDsl0 {
  type In0
  type In[_]<:In0
  type Out0
  type Out[_]<:Out0
  type Contact0 <: In0 with Out0
  type Contact[_]<:In[_]
  def contact[T](name: String): Contact[T]
}

trait SystemBuilderApi[Contact[_]] {

}

