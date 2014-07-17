Инструментарий описания составных типов synapse-frames
======================================================

Основные возможности
--------------------

1. Описание атрибутов сущности, привязанных к типу сущности. 
2. Описание иерархической схемы вложенных типов с возможностью проверки экземпляров на соответствие схеме.
3. Возможность полного отделения схемы от данных. Данные могут быть представлены в виде линейного списка.
4. Строго типизированное конструирование экземпляров данных. 
 
Пример описания простой схемы
-----------------------------

Пусть имеется некоторая иерархия типов, которая может включать type aliases, traits, classes, object.types
и т.д.:

<pre>
    abstract class Shape
    trait BoundingRectangle
    final class Rectangle extends Shape with BoundingRectangle
    final class Circle extends Shape with BoundingRectangle
</pre>

Мы можем привязать внешним образом свойства к любому типу:
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
При этом свойства, объявленные для родительских типов, могут свободно использоваться для дочерних типов.

Здесь мы свойства собрали в объект-наследник PropertySeq, что позволяет сразу получить полную 
схему соответствующего типа (CircleS.toSchema). Вместе с тем, можно совершенно свободно объявить свойство вне такого объекта:
<pre>
    val name = Rel[Shape, String]("name")
</pre>


Можно описать схемы, содержащие разные наборы свойств. Несмотря на то, что тип один, состав свойств 
может быть произвольным, в зависимости от потребностей приложения:  
<pre>
    val onlyBoundingRectSchema = BoundingRectangleS.toSchema
    
    val someInfoAboutACircle = record[Circle](radius)
    val fullInfoAboutACircle = someInfoAboutACircle ++ onlyBoundingRectSchema
</pre>

Экземпляры данных могут быть собраны с использованием билдера:
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

Также можно в runtime'е убедиться, что экземпляры соответствуют схемам:
<pre>
    assert(isMatching(shape10, someInfoAboutACircle). isEmpty)
    assert(isMatching(shape10, fullInfoAboutACircle). isEmpty)
    assert(isMatching(circ10 , someInfoAboutACircle). isEmpty)
    assert(isMatching(circ10 , fullInfoAboutACircle).nonEmpty)
</pre>

Текущая версия (на момент составления документации) - ru.primetalk:synapse-frames_2.10:1.3.3.