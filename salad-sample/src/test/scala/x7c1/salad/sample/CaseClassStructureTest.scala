package x7c1.salad.sample

import org.scalatest.{FlatSpecLike, Matchers}
import x7c1.salad.inspector.{TypeReflector, ObjectReflector}

class CaseClassStructureTest extends FlatSpecLike with Matchers {

  behavior of ObjectReflector.getClass.getSimpleName

  it can "inspect case class fields through apply-method" in {
    val outline = ObjectReflector.inspect(SampleCaseClass)
    val Some(signature) = outline.methods.find(_.decodedName == "apply")
    val arguments = signature.argumentsList.flatten

    val Some(foo) = arguments.find(_.name == "foo").headOption
    foo.typeDigest.typedName shouldBe "scala.Int"

    val Some(bar) = arguments.find(_.name == "bar").headOption
    bar.typeDigest.typedName shouldBe "java.lang.String"

    val Some(baz) = arguments.find(_.name == "baz").headOption
    baz.typeDigest.typedName shouldBe "scala.collection.Seq[scala.Long]"
  }

  behavior of TypeReflector.getClass.getSimpleName

  it can "inspect case class fields through public-methods" in {
    val digest = TypeReflector.inspect[SampleCaseClass]

    val Some(foo) = digest.members.find(_.decodedName == "foo").headOption
    foo.resultType.typedName shouldBe "scala.Int"

    val Some(bar) = digest.members.find(_.decodedName == "bar").headOption
    bar.resultType.typedName shouldBe "java.lang.String"

    val Some(baz) = digest.members.find(_.decodedName == "baz").headOption
    baz.resultType.typedName shouldBe "scala.collection.Seq[scala.Long]"
  }
}

class ClassStructureTest extends FlatSpecLike with Matchers {

  behavior of TypeReflector.getClass.getSimpleName

  it can "inspect class fields" in {
    val digest = TypeReflector.inspect[SampleClass]

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