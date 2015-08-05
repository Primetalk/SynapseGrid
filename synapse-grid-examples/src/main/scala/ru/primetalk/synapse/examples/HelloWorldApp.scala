package ru.primetalk.synapse.examples

import ru.primetalk.synapse.core._

/**
 * @author zhizhelev, 26.07.15.
 */
object HelloWorldApp extends App {

  /** Public interface of a HelloWorldSystem. */
  implicit class HelloWorldOuterInterface(ib: OuterInterfaceBuilder) {
    val nameInput = ib.input[String]("nameInput")
    val helloOutput = ib.output[String]("helloOutput")
  }

  implicit object HelloWorldSystemImpl extends SystemImplementation[HelloWorldOuterInterface] {
    override def apply(outer: HelloWorldOuterInterface)(implicit sb: SystemBuilder): Unit = {
      (outer.nameInput -> outer.helloOutput).map("Hello, "+_)
    }

  }

  /** The system adds "Hello, " prefix to the input String.*/
  object HelloSystem extends BaseTypedSystem{
    val nameInput = input[String]("nameInput")
    val helloOutput = output[String]("helloOutput")
    (nameInput -> helloOutput).map("Hello, "+_, "hello, + _")
  }

  val s = HelloSystem.toStaticSystem
  s.toDot().saveTo("helloSystem.dot")

  val f = s.toDynamicSystem.toMapTransducer(HelloSystem.nameInput, HelloSystem.helloOutput)
  val name = "World"

  val hello = f(name)
  println(hello)
}
