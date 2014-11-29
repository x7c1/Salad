package x7c1.salad.sample

import x7c1.salad.SaladType
import x7c1.salad.compile.TypeInspector
import x7c1.salad.reflect.TypeReflector

trait SampleTypes {
  def sampleType: SaladType
  def nestedPackage: SaladType
  def nestedInTrait: SaladType
  def nestedInObject: SaladType
}

object TypesByMacro extends SampleTypes{
  def sampleType = {
    TypeInspector.inspect[SampleType]
  }
  def nestedPackage = {
    TypeInspector.inspect[x1.x2.Nested]
  }
  def nestedInObject = {
    TypeInspector.inspect[x1.x2.FooObject.InnerObject]
  }
  def nestedInTrait = {
    TypeInspector.inspect[x1.x2.FooTrait#InnerTrait]
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
    TypeReflector.inspect[x1.x2.FooTrait#InnerTrait]
  }
  def nestedInObject = {
    TypeReflector.inspect[x1.x2.FooObject.InnerObject]
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
      trait InnerTrait
    }
    object FooObject {
      trait InnerObject
    }
  }
}
