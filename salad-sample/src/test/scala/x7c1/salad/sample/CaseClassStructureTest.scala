package x7c1.salad.sample

import org.scalatest.{FlatSpecLike, Matchers}
import x7c1.salad.inspector.ObjectReflector

class CaseClassStructureTest extends FlatSpecLike with Matchers {

  behavior of ObjectReflector.getClass.getSimpleName

  it can "inspect case class fields" in {
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

}

case class SampleCaseClass(foo: Int, bar: String, baz: Seq[Long])

