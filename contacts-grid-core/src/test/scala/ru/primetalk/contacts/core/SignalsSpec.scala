package ru.primetalk.contacts.core

import org.specs2.Specification

import UniSets._

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
  type `{In}` = Singleton[In]

  case object Out extends Contact {
    type Data = String
  }

  type Out = Out.type
  type `{Out}` = Singleton[Out]

  def f(i: Int): String = i.toString

  val liftedF: `{In}` >> `{Out}` = lift(In, Out)(f)

  def testLift = {
    val inputData = 42
    val expectedOutput = f(inputData)
    val inputSignal = signal[`{In}`](SignalOnContact(In)(inputData))
    val outputSignal = liftedF(inputSignal)
    val expectedOutputSignal = signal[`{Out}`](SignalOnContact(Out)(expectedOutput))

    outputSignal.head mustEqual expectedOutputSignal
  }

  def testSingleContact = {
    val identity = trivialLift(In)
    val inputData = 42
    val inputSignal = signal[`{In}`](SignalOnContact(In)(inputData))
    val outputSignal = identity(inputSignal)
    val expectedOutputSignal = signal[`{In}`](SignalOnContact(In)(inputData))
    outputSignal.head mustEqual expectedOutputSignal
  }
}
