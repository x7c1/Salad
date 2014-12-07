package x7c1.salad.sample

import x7c1.salad.inspector.{TypeDigest, TypeExpander, TypeReflector}

trait SampleTypes {
  def sampleType: TypeDigest
  def nestedPackage: TypeDigest
  def nestedInTrait: TypeDigest
  def nestedInObject: TypeDigest
}

object TypesByMacro extends SampleTypes{
  def sampleType = {
    TypeExpander.inspect[SampleType]
  }
  def nestedPackage = {
    TypeExpander.inspect[x1.x2.Nested]
  }
  def nestedInObject = {
    TypeExpander.inspect[x1.x2.FooObject.InObject]
  }
  def nestedInTrait = {
    TypeExpander.inspect[x1.x2.FooTrait#InTrait]
  }
}

object TypesByReflection extends SampleTypes {
  def sampleType = {
    TypeReflector.inspect[SampleType]
  }
  def nestedPackage = {
    TypeReflector.inspect[x1.x2.Nested]
  }
  def nestedInTrait = {
    TypeReflector.inspect[x1.x2.FooTrait#InTrait]
  }
  def nestedInObject = {
    TypeReflector.inspect[x1.x2.FooObject.InObject]
  }
}

trait SampleType {
  def intValue: Int
  def stringValue: String

  def listFloatValue: List[Float]
  def `quoted-1.2.3`: Int
  def genericValue: GenericDisplayType[String, GenericDisplayType[Int, Float]]
  def values: Seq[GenericDisplayType[String, Int]]
}

trait GenericDisplayType[A, B] {
  def foo(x: Double): Double
  def valueB: B
}

package x1 {
  package x2 {
    trait Nested {
      def foo: Int
    }
    trait FooTrait {
      trait InTrait
    }
    object FooObject {
      trait InObject
    }
  }
}
