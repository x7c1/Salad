package x7c1.salad.sample
import org.specs2.mutable.Specification

object TypeDisplayTest extends Specification {
  TypeStructureSample.getClass.getSimpleName should {
    "inspect a type structure" in {
      val x = TypeStructureSample.value1
      x.typeName === classOf[SampleMemberType].getName

      val Some(x1) = x.members.find(_.fieldName == "intValue")
      x1.resultType.typeName === "Int"

      val Some(x2) = x.members.find(_.fieldName == "listFloatValue")
      x2.resultType.typeName === "List[Float]"

      val Some(x3) = x.members.find(_.fieldName == "quoted-1.2.3")
      x3.resultType.typeName === "String"

      val Some(x4) = x.members.find(_.fieldName == "genericValue")
      x4.resultType.typeName ===
        "x7c1.salad.sample.GenericDisplayType[" +
          "String,x7c1.salad.sample.GenericDisplayType[Int,Float]]"

      val y = x4.resultType.members.head
      y.fieldName === "valueB"
      y.resultType.typeName ===
        "x7c1.salad.sample.GenericDisplayType[Int,Float]"
    }
  }
}
