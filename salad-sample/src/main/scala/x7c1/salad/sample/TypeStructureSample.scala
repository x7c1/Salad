package x7c1.salad.sample

import x7c1.salad.TypeStructure
import x7c1.salad.reflect.TypeReflection

object TypeStructureSample {
  def value1 = {
    TypeStructure.inspect[SampleMemberType]
  }
  def reflect = {
    TypeReflection.inspect[SampleMemberType]
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

