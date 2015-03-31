package x7c1.salad.sample

import org.scalatest.{FlatSpecLike, Matchers}

trait ClassCommonTests { this: FlatSpecLike with Matchers =>

  def types: SampleClasses

  behavior of types.getClass.getName

  it can "inspect case class fields" in {
    val digest = types.sampleCaseClass

    val Some(foo) = digest.members.find(_.decodedName == "foo").headOption
    foo.resultType.typedName shouldBe "scala.Int"

    val Some(bar) = digest.members.find(_.decodedName == "bar").headOption
    bar.resultType.typedName shouldBe "java.lang.String"

    val Some(baz) = digest.members.find(_.decodedName == "baz").headOption
    baz.resultType.typedName shouldBe "scala.collection.Seq[scala.Long]"
  }

  it can "inspect class fields" in {
    val digest = types.sampleClass

    val Some(foo) = digest.members.find(_.decodedName == "foo").headOption
    foo.resultType.typedName shouldBe classOf[GenericMixin].getName
    val Some(fooValue) = foo.resultType.members.find(_.decodedName == "genericValue1")
    fooValue.resultType.typedName shouldBe "scala.Int"

    val Some(bar) = digest.members.find(_.decodedName == "bar").headOption
    bar.resultType.typedName shouldBe "java.lang.String"

    val Some(baz) = digest.members.find(_.decodedName == "baz").headOption
    baz.resultType.typedName shouldBe "scala.collection.Seq[scala.Long]"

    val privateArg = digest.members.find(_.decodedName == "privateArg")
    privateArg shouldBe None

    val privateValue = digest.members.find(_.decodedName == "privateValue")
    privateValue shouldBe None

    val method = digest.members.find(_.decodedName == "hello")
    method shouldBe None

    val withParenthesis = digest.members.find(_.decodedName == "withParenthesis")
    withParenthesis shouldBe None
  }
}

class ClassReflectionTest extends FlatSpecLike with Matchers with ClassCommonTests{
  override def types = ClassesByReflection
}

class ClassMacroTest extends FlatSpecLike with Matchers with ClassCommonTests{
  override def types = ClassesByMacro
}
