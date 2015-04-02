package x7c1.salad.sample.factory

import x7c1.salad.factory.TypeFactory

import scala.language.reflectiveCalls

object TypeFactorySample {

  def Sample = TypeFactory[Sample]
  def val2 = {
    val x = Sample(
      value1 = 123,
      value2 = "hello",
      value3 = List("abc")
    )
    x.value2
  }

  def SubSample = TypeFactory[SubSample]
  def val4 = {
    val x = SubSample(
      value1 = 345,
      value2 = "world",
      value3 = List("def"),
      value4 = 1.23,
      value5 = 567
    )
    x.value4
  }

  def val5 = {
    val GenericSample1 = TypeFactory[GenericSample1[String]]
    val x = GenericSample1(genericValue1 = "foo5")
    x.genericValue1
  }

  def val7 = {
    val GenericMixin = TypeFactory[GenericMixin]
    val x = GenericMixin(
      genericValue1 = 777,
      genericValue2 = "bar"
    )
    x.genericValue1
  }
}

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

trait GenericSample1[A] {
  def genericValue1: A
}

trait GenericSample2[X] {
  def genericValue2: X
}

trait GenericMixin extends GenericSample1[Int] with GenericSample2[String]
