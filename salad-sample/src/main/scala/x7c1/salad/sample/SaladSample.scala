package x7c1.salad.sample

import x7c1.salad.Salad.Factory
import scala.language.reflectiveCalls

object SaladSample {

  def Sample = Factory[Sample]
  def val2 = {
    val x = Sample(
      value1 = 123,
      value2 = "hello",
      value3 = List("abc")
    )
    x.value2
  }

  def SubSample = Factory[SubSample]
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
