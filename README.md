SynapseGrid
===========

A few more words about what SynapseGrid is:

- framework for constructing systems that are:
  - reactive
  - event-driven
  - resilent  
- it resembles other modern event-driven architectures (Akka, Storm, Hadoop).
- it is probably one of the finest grained frameworks. The building block is as simple as a function!

Feature highlights
------------------

1. SynapseGrid allows function composition far more flexible than monads.
2. Strictly typed message handling in Akka actors (a bit more natural than in Typed actors or Typed Channels).
3. Multi input/multi output functions.
4. Systems process portions of information ASAP.
5. It is possible to nest subsystems (like matreshkas).
6. Declarative composition in the form of DataFlow diagram.
7. Easy to use DSL:
<pre>
  val a = contact\[String]\("a")
  val b = contact\[String]\("b")
  val c = contact\[Char]\("c")
  a -> b flatMap (_.split("\\s+"))
  a -> c flatMap (_.toCharArray)
  inputs(a)
  outputs(b,c)
</pre>
8. Dependency injection replacement (accompanied with Scala traits).

![example1 system picture](docs/images/example1.png)
![example2 system picture](docs/images/example2.png)
![example3 system picture](docs/images/example3.png)

For details see [README in English](docs/README.EN.md).

See also
--------
1. [Walkthrough in English](docs/README.EN.md) English readme.
2. [README по-русски](docs/README.RU.md)
3. [Motivation for SynapseGrid](docs/SpeechPortalMotivation.RU.md)
4. [License (BSD-like)](LICENSE.md)
5. [Actors support (in Russian)](Actors.RU.md).
