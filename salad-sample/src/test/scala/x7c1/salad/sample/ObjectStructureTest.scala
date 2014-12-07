package x7c1.salad.sample
import org.specs2.mutable.Specification

object ObjectStructureTest extends Specification {
  "inspect object methods" in {
    val x = ObjectInspectorByReflection.sample
    val Some(method) = x.methods.find(_.decodedName == "sampleMethod")
    method.resultType.typedName === "scala.collection.Seq[x7c1.salad.sample.SampleType]"

    x.methods.find(_.decodedName == "privateMethod") === None
  }
}
