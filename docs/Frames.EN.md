The tool for incomplete data representation - synapse-frames
============================================================

Key features
------------

1. Entity attribute declaration for arbitrary types. 
2. Natural type hierarchy declaration (using all Scala's features). 
3. Complete separation of data and schema. The data can be stored in a flat List[Any] or Array<Object> and
then checked and converted to a strict typed construction.
4. Typed representation of instances.
 
A simple schema example
-----------------------

Let's have a few types (that can include type aliases, traits, classes, object.types, etc.):

<pre>
    abstract class Shape
    trait BoundingRectangle
    final class Rectangle extends Shape with BoundingRectangle
    final class Circle extends Shape with BoundingRectangle
</pre>

We can bind properties to any type:
<pre>
    object BoundingRectangleS extends PropertySeq[BoundingRectangle] {
      val width = simpleProperty[Int]("width")
      val height = simpleProperty[Int]("height")
    }
    import BoundingRectangleS._

    object CircleS extends PropertySeq[Circle] {
      importProperties(BoundingRectangleS.toSchema)
      val radius = Rel[Circle, Int]("radius")
    }
    import CircleS._

</pre>
Properties bound to parent types can be used freely for descendent types.

In the above example we have collected properties within a PropertySeq descendent. This allows us
to immediately have full schema of corresponding type (CircleS.toSchema). Also we can declare a 
property outside of the schema-object:
<pre>
    val name = Rel[Shape, String]("name")
</pre>

It is possible to have different schemas bound to the same type depending on the application requirements.
<pre>
    val onlyBoundingRectSchema = BoundingRectangleS.toSchema
    
    val someInfoAboutACircle = record[Circle](radius)
    val fullInfoAboutACircle = someInfoAboutACircle ++ onlyBoundingRectSchema
</pre>

Data instances (records, frame instances) can be constructed using builder:
<pre>
    val circ10 = new Builder(someInfoAboutACircle).
      set(radius, simple(10)).
      toInstance
    val shape10 = new Builder(fullInfoAboutACircle).
      fillFromInstance(circ10).
      set(width, simple(10)).
      set(height, simple(10)).
      toInstance
    assert(shape10.get(radius) === circ10.get(radius))
</pre>

At runtime one can make sure that some instances meets a schema:
<pre>
    assert(isMatching(shape10, someInfoAboutACircle))
    assert(isMatching(shape10, fullInfoAboutACircle))
    assert(isMatching(circ10 , someInfoAboutACircle))
    assert(nonMatching(circ10 , fullInfoAboutACircle))
</pre>

Getting started
---------------
The current version of the library is "ru.primetalk:synapse-frames_2.10:1.3.3". 