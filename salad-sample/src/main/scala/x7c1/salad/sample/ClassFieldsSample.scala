package x7c1.salad.sample

import x7c1.salad.inspector.TypeDigest
import x7c1.salad.inspector.macros.TypeExpander
import x7c1.salad.inspector.reflect.TypeReflector

trait SampleClasses {
  def sampleClass: TypeDigest
  def sampleCaseClass: TypeDigest
}

object ClassesByReflection extends SampleClasses {
  override def sampleClass = TypeReflector.inspect[SampleClass]
  override def sampleCaseClass = TypeReflector.inspect[SampleCaseClass]
}

object ClassesByMacro extends SampleClasses {
  override def sampleClass = TypeExpander.inspect[SampleClass]
  override def sampleCaseClass = TypeExpander.inspect[SampleCaseClass]
}

case class SampleCaseClass(foo: Int, bar: String, baz: Seq[Long])

class SampleClass(
  val foo: GenericMixin,
  val bar: String,
  val baz: Seq[Long],
  privateArg: Int ){

  def hello: String = "world"

  def withParenthesis(): Int = ???

  private def privateValue: Int = ???
}
