package x7c1.salad.sample
import org.specs2.mutable.Specification

object TypeDisplayTest extends Specification {
  TypeStructureSample.getClass.getSimpleName should {
    "inspect a type structure" in {
      val x = TypeStructureSample.value1
      x.typedName === classOf[SampleMemberType].getName

      val Some(x1) = x.members.find(_.decodedName == "intValue")
      x1.resultType.typedName === "Int"

      val Some(x2) = x.members.find(_.decodedName == "listFloatValue")
      x2.resultType.typedName === "List[Float]"

      val Some(x3) = x.members.find(_.decodedName == "quoted-1.2.3")
      x3.resultType.typedName === "String"

      val Some(x4) = x.members.find(_.decodedName == "genericValue")
      x4.resultType.typedName ===
        "x7c1.salad.sample.GenericDisplayType[" +
          "String,x7c1.salad.sample.GenericDisplayType[Int,Float]]"

      val y = x4.resultType.members.head
      y.decodedName === "valueB"
      y.resultType.typedName ===
        "x7c1.salad.sample.GenericDisplayType[Int,Float]"
    }
  }
}
