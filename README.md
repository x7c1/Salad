#Salad

Library using Scala macros and reflections.

## Examples

### Traversing type structures

In case of definitions like below:

```scala
package example

trait SampleStructure {
  def x: Int
  def y: SampleGeneric[String, Long, Int]
  def z: List[Int]
}
trait SampleGeneric [A, B, C]{
  def a: A
  def b: B
  def c: Nested[Nested[Nested[C]]]
}
trait Nested[A]{
  def foo: A
}
```

You can scan each fields on this by `TypeReflector`.

```scala
val digest = TypeReflector.inspect[SampleStructure]

println(digest.members.find(_.decodedName == "z").map(_.resultType.typedName))
// Some(scala.collection.immutable.List[scala.Int])

val name = for {
  y <- digest.members.find(_.decodedName == "y")
  b <- y.resultType.members.find(_.decodedName == "b")
} yield {
  b.resultType.typedName
}
println(name)
// Some(scala.Long)
```

It also makes so easy to scan fields recursively.

```scala
println(dump(digest))
/*
example.SampleStructure
  z : scala.collection.immutable.List[scala.Int]
  y : example.SampleGeneric[java.lang.String,scala.Long,scala.Int]
    c : example.Nested[example.Nested[example.Nested[scala.Int]]]
      foo : example.Nested[example.Nested[scala.Int]]
        foo : example.Nested[scala.Int]
          foo : scala.Int
    b : scala.Long
    a : java.lang.String
  x : scala.Int
 */

def dump(digest: TypeDigest, indent: Int = 1): String = {
  val lines = digest.typedName +:
    digest.members.
      map { f => s"${f.decodedName} : ${dump(f.resultType, indent + 1)}" }.
      map { "  " * indent + _ }

  lines.mkString("\n")
}
```

### Factory for abstract traits

First, define type structures like:

```scala
trait Sample{
  def value1: Int
  def value2: String
  def value3: List[String]
}
trait SubSample extends Sample with SampleTrait{
  def value4: Double
}
trait SampleTrait {
  def value5: Int
}
```

Then you can generate its factory automatically by `TypeFactory`.

```scala
val SubSample = TypeFactory[SubSample]
val x = SubSample(
  value1 = 345,
  value2 = "world",
  value3 = List("def"),
  value4 = 1.23,
  value5 = 567
)
x.value4// 1.23
```

Boring boilerplate is not required now, of course, to generic types.

```scala
trait GenericSample1[A] {
  def genericValue1: A
}
trait GenericSample2[X] {
  def genericValue2: X
}
trait GenericMixin extends GenericSample1[Int] with GenericSample2[String]
```

Perfectly simple.

```scala
val GenericMixin = TypeFactory[GenericMixin]
val x = GenericMixin(
  genericValue1 = 777,
  genericValue2 = "bar"
)
x.genericValue1// 777
```

## Usage

In your sbt build definition:

```scala
lazy val `your-app` = project.
  dependsOn(ProjectRef(uri("git://github.com/x7c1/Salad.git#0.1"), "salad-lib"))
```

Replace the tag in `Salad.git#0.1` with [latest version](https://github.com/x7c1/Salad/releases).

##License

Salad is released under the MIT license

* http://www.opensource.org/licenses/MIT
