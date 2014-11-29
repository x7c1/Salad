package x7c1.salad.sample

import x7c1.salad.TypeStructure
import x7c1.salad.reflect.TypeReflection

object TypeStructureSample {
  def value1 = {
    TypeStructure.inspect[SampleMemberType]
  }
  def nested = {
    TypeStructure.inspect[x1.x2.Nested]
  }
  def nestedInObject = {
    TypeStructure.inspect[x1.x2.FooObject.InnerObject]
  }
  def nestedInTrait = {
    TypeStructure.inspect[x1.x2.FooTrait#InnerTrait]
  }

  def reflect = {
    TypeReflection.inspect[SampleMemberType]
  }
  def reflectNestedPackage = {
    TypeReflection.inspect[x1.x2.Nested]
  }
  def reflectInTrait = {
    TypeReflection.inspect[x1.x2.FooTrait#InnerTrait]
  }
  def reflectInObject = {
    TypeReflection.inspect[x1.x2.FooObject.InnerObject]
  }
}

trait SampleMemberType {
  def intValue: Int
  def listFloatValue: List[Float]
  def `quoted-1.2.3`: String
  def genericValue: GenericDisplayType[String, GenericDisplayType[Int, Float]]
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
      trait InnerTrait
    }
    object FooObject {
      trait InnerObject
    }
  }
}
