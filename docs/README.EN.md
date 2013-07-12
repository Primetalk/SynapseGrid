SinapseGrid contact system
==========================

Key features
------------
1. SinapseGrid provides better opportunity for function composition, which goes far beyond from monad capabilities.
2. Mutual usage with a Akka-actors allows strictly typed data processing, that significantly excels Typed actors.
3. Composite functions, that have multiple inputs and outputs.

(Reasons, that led us to create SinapseGrid could be found here: [Потребности систем ведения диалога](docs/SpeechPortalMotivation.RU.md). )

Breadboard concept
------------------
Contact System is based on some principles, that are lightened here.

Imagine a breadboard for assembling an electronic circuit.
There is bunch of apertures and contact areas. Some contacts are arranged for apt electronic device connections while others are auxiliary, and used to link components

There are different components (impedors, transistors), installed on the breadboard.
There are also subsystems like: power supply, low frequency amplifier, filter and etc.
Some subsystems may stay untapped, some contacts and subsystems may not be involved.
There may be not connected components to them. Some power supply outputs may stay unclaimed. Or some circuit's inputs may be unused.

Breadboard can be a good illustration to contact system.


Contacts and links
---------------------
The Contact is an object with Contact[T] type and with a name, that usually matches variable name.
It's very easy to create a simple instance of Contact. You may see example listed below (All examples are written Scala)

<pre>
	val myContact = contact[String]("myContact")
</pre>

Contact that doesn't contain any data, because it designates a point on the breadboard.
It may have component's IO connections of type String.
The component that has one input and one output is called an arrow or an link.
In the most wonted way, an ordinary function can be applied as component. Like in example below:

<pre>
	def getLength(s:String) = s.length
</pre>

Let's write a code, that calculates length for every incoming string.

<pre>
	val len = contact[Int]( "len" )
	myContact -> len map getLength
</pre>

or, briefly

<pre>
	val len = myContact.map( _.length )
</pre>

The system sample is listed below (to make testing possible, additional contacts were connected: input, output).

![example1 system picture](images/example1.png)

(In the last case, the len contact of following type (Int) will be created automatically.)


Data processing
---------------

The code above, doesn't make any job, because contacts either functions don't store any data.
This code only declares system structure - Contacts and their bonds with components or subsystems.

External binding is used to attach data to some contact.
It means, that some object will be created. This Object will store link, contact  and data bound to corresponding contact.
In Concat System terminology this object will be called a Signal.

<pre>
	case class Signal[T](contact:Contact[T], data:T)
</pre>

(You may also meet terms, that corresponds to a signal like: Event, Data, Frame and Message.)
System state will be represented as list of signals, that stored on different contacts in one discrete time moment.

<pre>
	type Signals = List[Signal[_]]
</pre>

SignalProcessor performs functional transformation of the original list of signals to it's finite state.
Each signal is transmitted to the input queue, that belong to each component and  it's connected to the corresponding contact.
Signal, received after contact's job completion, will be  added to the list of the next timing signal.
Signal Processor exits, when all signals are processed by previous points of time

The theory of hidden Markov models has a good notion of trellis (the time chart of signal constellation).
SignalProcessor is used to build  a trellis, based on input data.

When the trellis building stops?
If the process does not stop, then all data will reach the outer contacts and, as there are no connected components, all data will be lost.
To avoid this, output contacts are specified in system description.

<pre>
	outputs(len) // outputs(output1, output2, output3)
</pre>

therefore, processing will stop, when all signals in the current list will belong to output contacts.


Arrow types
-----------

The most common situation in signal processing, when 0 or more input elements generated per one element
Handling such situation in Scala, could be performed via higher-order function called flatMap.
That's why an arrows, annotated by functions, that return 0..n elements has a FlatMap type.

<pre>
	val wordsContact = someStringContact.flatMap(_.split("\\s+".r))
</pre>

System will look like this:

![example2 system picture](images/example2.png)

An important case of FlatMap arrows are 0..1 arrows, which reflect (sometimes they doesn't) data, that depends on certain conditions.
There's also a method, dedicated for arrows creation. It's called a filter:

<pre>
	val nonEmptyString = myContact.filter(_.length>0)
</pre>


For-comprehension compatibility
-------------------------------

An interesting feature of the Scala is ability to use syntactic sugar for custom methods.
Methods like map, flatMap, filter or withFilter, are already announced, so, it's possible to use a for-comprehension:

<pre>
	val helloContact = for {
	   s <- myContact
	   if s.length >0
	} yield "Hello, "+s
</pre>

The same code is you may below (iе contains two arrows):

<pre>
	val helloContact = myContact.filter(s => s.length>0).map(s=>"Hello, "+s)
</pre>

In some cases, when processing algorithm branches a lot, this syntax looks pretty good.

Working with state
------------------

Till now, all examples operated only with data, that was coming to the input contact.Result wasn't stored or transmitted anywhere.
We used "pure" immutable functions without side-effects. This functions has a lot of useful characteristics. For example, we could easily parallel processing.
There's no need to recreate system to perform another data processing — on-start creation will be enough.
There's no need to debug systems like this - absence of inside state and side-effects, makes determinate (defined only by input data) result.

If data processing logic requires state save - the most obvious solution to use variable inside function to store state.
For instance:

<pre>
	var counter = 0
	val helloCount = myContact.map({any => 	counter += 1;  counter})
</pre>

This will work, alas we're losing all advantages of immutable system.

But what if we will store the state separate from the system? And then, in the right time before function call, state will be executed and then put back.

How to work with state, stored somewhere? Function has to accept current state on input and return new value.

<pre>
	val helloCount = myContact.[link to variable, where counter state is stored].map({(any, counter) => (counter+1, counter + 1)})
</pre>

Let's take a closer look to this function. We'll make it verbose via def;

<pre>
	def incCounter(any:String, counter:Int) : (Int, Int) = {
	  val newCounterValue = counter+1
	  val resultOfThisFunction = newCounterValue
	  return (resultOfThisFunction, newCounterValue)
	}
</pre>

The function, that process the state is pure. Q.e.d.

Now, it only remains to determine how easily to store and retrieve state.

We will use StateHandle[T] (some sort of Contact), to identify different state variables

<pre>
	val counterS = state[Int]("counterS", 0)
	val helloCount = contact[Int]("helloCount")
</pre>

This identifier contains variable type, name, and initial value.

Current state value is not available at update. Actually it's not stored anywhere.
Looking ahead a little bit, SignalProcessor stores current all variables values in Map

To use this state in our helloCounter function, we have to refer it.

<pre>
    (myContact.withState(counterS) -> helloCount).stateMap({(counter: Int, any:String) => (counter + 1, counter + 1)},"inc "+counterS)
	val helloCount = myContact.stateMap(counterS, {(any, counter) => (counter+1, counter + 1)})
</pre>

It looks a little bit cumbersome, but we have all pure functions advantages.

![example3 system picture][example3]

[example3]: images/example3.png "System example #3"

DSL has a set of auxiliary high-order functions, that simplify working with states.


Drawing the system scheme
-------------------------

Since we have a declarative system, there is a great chance to study and analyse it through e a system graph.

To get system's image, toDot call will be sufficient.
This method traverses all system elements (contacts, arrows, subsystems) and generates a .dot text file.


You can view .dot files via XDot, or any other software.
All pictures in `images` folder were obtained by:

<pre>
    dot -Tpng example3.dot > example3.png
</pre>


System constructing via SystemBuilder
-------------------------------------
All examples, of arrows/contacts creation must be stored in some class/trait, that extends SystemBuilder.
It have to contain basic methods, that allow you to incrementally crate contacts or different sort of arrows.
SystemBuilder – is a mutable class. It doesn't participate in runtime-processing.

!!!!!
To get a clear system description, constructed by SystemBuilder, that's will be enough to call toStaticSystem method.
This method returns simple immutable case-class, which contains all contacts and arrows.

There are bunch of DLS's that stored in separate traits, to use them, you have to connect them to your Builder.

Instead of obvious way of system construction

<pre>
	val sb = new SystemBuilderC("MySystem")
	import sb._
	...
	val system = sb.toStaticSystem
</pre>

you could also extend a trait

<pre>
	trait MySystemBuilder extends SystemBuilder {
	  // setSystemName("MySystem") 
	  ...
	}

	val system = new MySystemBuilder.toStaticSystem
</pre>

After receiving StaticSystem, it can be directly used for signal processing via SignalProcessor.
By this the system state will transmit the input SignalProcessor and remember on return all the time.
There is DynamicSystem ( DynamicSystem == StaticSystem + State) class, can be used to ease state manage,

You can use this class as an ordinary function (but bear in mind that has the state, hidden inside, and the function has a side effect).


Subsystems
----------
As the program size increases, there is a need to allocate some subsystem blocks in reuse purposes.
Use the addSubsystem method to add subsystem.
Since the subsystem has a state, stateHandle is also indicated.

>! TODO:Add stateHandle description

<pre>
	val subsystem = new MySubsystemBuilder.toStaticSystem
	val s1 = state[SystemState]("s1", subsystem.s0)
	sb.addSubsystem(subsystem, s1)
</pre>

To make subsystem able to get an input data, some of it's contacts must be declared as input:

<pre>
	inputs(input1, input2)
</pre>

in this case, all data that appears in external system and on apt contacts will be processed by subsystem.

If there is a need to connect a several instances of subsystem, you would like to bind them to different input/output contacts.
For this purpose, you should use a subsystem embedded in another subsystem. In the intermediate subsystem inputs link inputs, and outputs link outputs.
To do this, Builder intermediate subsystem uses methods mappedInput, mappedOutput, inputMappedTo, mapToOutput.
These methods enable the wiring creation, that proves connection between the external system contacts and the internal system contacts.


Akka Actors usage
-----------------

All, described above, systems are single-threaded. There are also many possible ways to achieve multithreading.
One of them – create an actor-based system, that will be fully compatible with Akka.
When Actor receives a Signal message, then it will be proceed in the most obvious way: signal will be sent next to embedded DynamicSystem.

The NonSignalWithSenderInput contact can be used for compatibility with programs, which doesn't support Signals.
This contact has (ActorRef, Any) type. It's first element will contain received data sender, the second – data.


1. [Read more about Actor support](Actors.EN.md).

