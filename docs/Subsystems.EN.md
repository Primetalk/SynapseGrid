Subsystems
==========

SynapseGrid allows creation of subsystem modules that interconnects. There are a few possible
ways to create the link between subsystems that have their pros and cons:

1. Shared contacts that are used directly within Builders
2. Passing outer system contact via constructor of the Builder.

For relatively small unique subsystems the shared contacts approach is the easiest way to transfer data
between subsystems. The shared contact can be documented and put into the common scope for two
subsystems (public).

However, when the system grows shared contacts has a few drawbacks:

1. Refactoring is more difficult.
2. It is much more difficult to use a few instances of a single system.

> How to connect a few instances of a system using shared contacts
> ----------------------------------------------------------------
>
> In this can it is usually required that the instances are connected to different parent system's
> contacts. This can be achieved by nesting the reused subsystem in a "wiring" system that
> has new unique contacts and connects inputs to inner inputs and inner outputs to outputs.
> The Builder of the intermediate subsystem has methods mappedInput, mappedOutput, inputMappedTo, mapToOutput.
> These methods makes the wiring easier.

3. Shared contacts are spread all around the project.
4. Shared contacts usually have public access level.

If public interface contacts are declared in Builder's constructor then the subsystem can be
connected to any contact of corresponding type. Inside the Builder actual contact is not used. Thus
we get the following benefits:
- a refactoring is much easier.
- instantiation of a few subsystem instances is done without nesting auxiliary subsystems.
- shared contacts can be defined inside the parent system builder and has restricted access level.

Public system interface
-----------------------

<pre>
    class MySystem {
        val input1 = new Contact[Int]("input1")
        val output1 = new Contact[Int]("output1")
        private val system = new MySystemBuilder(this).toStaticSystem
	    def toStaticSystem = system
	}

	class MySystemBuilder(outerInterface:MySystem) extends SystemBuilder {
	    import outerInterface._
	    inputs(input1)
	    outputs(output1)
	    input1.map(_ * 2) >> output1
	}
</pre>

It is easy to use the system in the parent system builder:

<pre>
    class ParentSystemBuilder extends SystemBuilder {
        val mySubsystem1 = addSubsystem(new MySystem)
        mySubsystem1.output1.foreach(println)
        someOtherContact >> mySubsystem1.input1

        val mySubsystem2 = addSubsystem(new MySystem)
        addSubsystem(mySubsystem2)
        mySubsystem2.output1.foreach(println)

    }

</pre>

