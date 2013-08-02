SynapseGrid
===========

Feature highlights
------------------

1. SynapseGrid allows function composition far more flexible than monads.
2. Strictly typed message handling in Akka actors (a bit more natural than in Typed actors or Typed Channels).
3. Multi input/multi output functions.
4. Systems process portions of information ASAP.
5. Declarative composition in the form of DataFlow diagram.
6. Easy to use DSL:
<pre>
  val a = contact\[String]\("a")
  val b = contact\[String]\("b")
  val c = contact\[Char]\("c")
  a -> b flatMap (_.split("\\s+"))
  a -> c flatMap (_.toCharArray)
  inputs(a)
  outputs(b,c)
</pre>
7. Dependency injection replacement (accompanied with Scala traits).

![example1 system picture](docs/images/example1.png)
![example2 system picture](docs/images/example2.png)
![example3 system picture](docs/images/example3.png)

For details see [README in English](docs/README.EN.md).

See also
--------
1. [README in English](docs/README.EN.md) English readme.
2. [README по-русски](docs/README.RU.md)
3. [Motivation for SynapseGrid](docs/SpeechPortalMotivation.RU.md)
4. [License (BSD-like)](LICENSE.md)
5. [Actors support (in Russian)](Actors.RU.md).
