package x7c1.salad.sample

import x7c1.salad.TypeStructure

object TypeStructureSample {

  def value1 = {
    TypeStructure.inspect[SampleMemberType]
  }
}

trait SampleMemberType {
  def intValue: Int
  def listFloatValue: List[Float]
  def `quoted-1.2.3`: String
  def genericValue: GenericDisplayType[String, GenericDisplayType[Int, Float]]
}

trait GenericDisplayType[A, B] {
  def valueB: B
}
