package ru.primetalk.contacts.core

import org.specs2.Specification

import TypeSets._

class SignalsSpec extends Specification with MySignals {

  def is =
    s2"""
  Methods of SignalsSpec should pass tests:
    - lift should yield expected results $testLift
    - trivial lift should not change signal $testSingleContact
  """

  case object In extends Contact with Serializable {
    type Data = Int
  }

  type In = In.type

  case object Out extends Contact {
    type Data = String
  }

  type Out = Out.type

  def f(i: Int): String = i.toString

  val liftedF: SignalOnContacts[In +: ∅] => Iterable[SignalOnContacts[Out +: ∅]] = lift(In, Out)(f)

  def testLift = {
    val inputData = 42
    val expectedOutput = f(inputData)
    val inputSignal = MySignalOps.wrap[In, In +: ∅](In, inputData)
    val outputSignal: Iterable[SignalOnContacts[Out +: ∅]] = liftedF(inputSignal)
    val expectedOutputSignal = MySignalOps.wrap[Out, Out +: ∅](Out, expectedOutput)

    outputSignal.head mustEqual expectedOutputSignal
  }

  def testSingleContact = {
    val identity = trivialLift(In)
    val inputData = 42
    val inputSignal = MySignalOps.wrap[In, In +: ∅](In, inputData)
    val outputSignal: Iterable[SignalOnContacts[In +: ∅]] = identity(inputSignal)
    val expectedOutputSignal = MySignalOps.wrap[In, In +: ∅](In, inputData)
    outputSignal.head mustEqual expectedOutputSignal
  }
}
