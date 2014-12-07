package x7c1.salad.sample
import org.specs2.mutable.Specification

object ObjectStructureTest extends Specification {
  "inspect object methods" in {
    val x = ObjectInspectorByReflection.sample
    x.fullName === "x7c1.salad.sample.SampleObject"
    x.decodedName === "SampleObject"

    val Some(method) = x.methods.find(_.decodedName == "sampleMethod")
    method.resultType.typedName === "scala.collection.Seq[x7c1.salad.sample.SampleType]"

    val Some(arg) = method.argumentsList.head.find(_.name == "sampleArg")
    arg.typeDigest.typedName === "x7c1.salad.sample.SampleType"

    val Some(x2) = arg.typeDigest.members.find(_.decodedName == "listFloatValue")
    x2.resultType.typedName === "scala.collection.immutable.List[scala.Float]"

    x.methods.find(_.decodedName == "privateMethod") === None
  }
}
