package x7c1.salad.sample

import org.scalatest.{FlatSpecLike, Matchers}

class ObjectStructureTest extends FlatSpecLike with Matchers {
  it can "inspect object methods" in {
    val x = ObjectInspectorByReflection.sample
    x.fullName shouldBe "x7c1.salad.sample.SampleObject"
    x.decodedName shouldBe "SampleObject"

    val Some(method) = x.methods.find(_.decodedName == "sampleMethod")
    method.resultType.typedName shouldBe "scala.collection.Seq[x7c1.salad.sample.SampleType]"

    val Some(arg) = method.argumentsList.head.find(_.name == "sampleArg")
    arg.typeDigest.typedName shouldBe "x7c1.salad.sample.SampleType"

    val Some(x2) = arg.typeDigest.members.find(_.decodedName == "listFloatValue")
    x2.resultType.typedName shouldBe "scala.collection.immutable.List[scala.Float]"

    x.methods.find(_.decodedName == "privateMethod") shouldBe None
  }
}
