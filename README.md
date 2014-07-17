SynapseGrid
===========

A few words about what SynapseGrid is:

- framework for constructing systems that are:
  - reactive
  - event-driven
  - resilent  
- it resembles other modern event-driven architectures (Akka, Storm, Hadoop).
- it is probably one of the finest grained frameworks. The building block is as simple as a function!

[Blog about SynapseGrid](http://synapse-grid.primetalk.ru/)

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
9. DataFlow diagram for a system can be created easily — system.toDot:
![example1 system picture](docs/images/example1.png)
![example2 system picture](docs/images/example2.png)
![example3 system picture](docs/images/example3.png)

For details see [README in English](docs/README.EN.md).

See also (English)
--------
1. [Walkthrough](docs/README.EN.md).
2. [Motivation for SynapseGrid](docs/SpeechPortalMotivation.RU.md).
3. [License (BSD-like)](LICENSE.md).
4. [Subsystems](docs/Subsystems.EN.md).
5. [Blog about SynapseGrid](http://synapse-grid.primetalk.ru/)
6. [Distrubuted systems](docs/Distributed.EN.md)
7. [Typed frames](docs/Frames.EN.md).

См. также (See also in Russian)
---------
1. [README по-русски](docs/README.RU.md).
2. [Actors support (in Russian)](docs/Actors.RU.md).
3. [Лицензия](docs/LICENSE.RU.md).
4. [Работа с подсистемами](docs/Subsystems.RU.md).
5. [Blog about SynapseGrid (en)](http://synapse-grid.primetalk.ru/)
6. [Распределённые системы](docs/Distributed.RU.md).
7. [Строго типизированные фреймы](docs/Frames.RU.md).
8. [Хабрахабр: Строго типизированное представление неполных данных](http://habrahabr.ru/post/229035/)
