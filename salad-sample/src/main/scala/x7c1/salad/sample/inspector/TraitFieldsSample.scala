package x7c1.salad.sample.inspector

import x7c1.salad.inspector.TypeDigest
import x7c1.salad.inspector.macros.TypeExpander
import x7c1.salad.inspector.reflect.TypeReflector
import x7c1.salad.sample.inspector.x1.x2.{Nested, FooTrait, FooObject}

trait SampleTypes {
  def sampleType: TypeDigest
  def sampleType2: TypeDigest
  def mergedType: TypeDigest

  def nestedPackage: TypeDigest
  def nestedInTrait: TypeDigest
  def nestedInObject: TypeDigest
}

object TypesByMacro extends SampleTypes{
  def sampleType = {
    TypeExpander.inspect[SampleType]
  }
  def sampleType2 = {
    TypeExpander.inspect[SampleType2]
  }
  def mergedType = {
    TypeExpander.inspect[MergedType]
  }
  def nestedPackage = {
    TypeExpander.inspect[Nested]
  }
  def nestedInObject = {
    TypeExpander.inspect[FooObject.InObject]
  }
  def nestedInTrait = {
    TypeExpander.inspect[FooTrait#InTrait]
  }
}

object TypesByReflection extends SampleTypes {
  def sampleType = {
    TypeReflector.inspect[SampleType]
  }
  def sampleType2 = {
    TypeReflector.inspect[SampleType2]
  }
  def mergedType = {
    TypeReflector.inspect[MergedType]
  }
  def nestedPackage = {
    TypeReflector.inspect[Nested]
  }
  def nestedInTrait = {
    TypeReflector.inspect[FooTrait#InTrait]
  }
  def nestedInObject = {
    TypeReflector.inspect[FooObject.InObject]
  }
}

trait MergedType {
  def value: InnerMergedType[String]
}

trait InnerMergedType[A] extends
  TypeToMixin1[A, Int] with
  TypeToMixin2[A, Long]

trait TypeToMixin1 [T, U]{
  def foo: T
  def bar: U
}

trait TypeToMixin2 [R, S]{
  def foo: R
  def baz: S
}

trait SampleType {
  def intValue: Int
  def stringValue: String

  def listFloatValue: List[Float]
  def `quoted-1.2.3`: Int
  def genericValue: GenericType[String, GenericType[Int, Float]]
  def values: Seq[GenericType[String, Int]]
}

trait GenericType[X, Y] {
  def foo(x: Double): Double
  def valueB: Y
}

trait GenericType2[S, Q <: S]{
  def genericFunction: S => Q
  def genericSeq: GenericType[S, Seq[Q]]
}

trait SampleType2 {
  def genericValue: GenericType2[SampleInnerType2, SampleInnerSubType2]
}

trait SampleInnerType2

trait SampleInnerSubType2 extends SampleInnerType2

package x1.x2 {
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
