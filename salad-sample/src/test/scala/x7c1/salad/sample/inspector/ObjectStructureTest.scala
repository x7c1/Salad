package x7c1.salad.sample.inspector

import org.scalatest.{FlatSpecLike, Matchers}
import x7c1.salad.inspector.reflect.ObjectReflector

class ObjectStructureTest extends FlatSpecLike with Matchers {

  behavior of ObjectReflector.getClass.getSimpleName

  it can "inspect object methods" in {
    val x = ObjectInspectorByReflection.sample
    x.fullName shouldBe "x7c1.salad.sample.inspector.SampleObject"
    x.decodedName shouldBe "SampleObject"

    val Some(method) = x.methods.find(_.decodedName == "sampleMethod")
    method.resultType.typedName shouldBe "scala.collection.Seq[x7c1.salad.sample.inspector.SampleType]"

    val Some(arg) = method.argumentsList.head.find(_.name == "sampleArg")
    arg.typeDigest.typedName shouldBe "x7c1.salad.sample.inspector.SampleType"

    val Some(x2) = arg.typeDigest.members.find(_.decodedName == "listFloatValue")
    x2.resultType.typedName shouldBe "scala.collection.immutable.List[scala.Float]"

    x.methods.find(_.decodedName == "privateMethod") shouldBe None
  }

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
}
