package x7c1.salad.sample
import org.specs2.mutable.Specification
import x7c1.salad.reflect.TypeReflection

object TypeStructureTest extends Specification {
  TypeStructureSample.getClass.getSimpleName should {
    "inspect a type structure by macro" in {
      val x = TypeStructureSample.value1
      x.typedName === classOf[SampleMemberType].getName

      val Some(x1) = x.members.find(_.decodedName == "intValue")
      x1.resultType.typedName === "Int"

      val Some(x2) = x.members.find(_.decodedName == "listFloatValue")
      x2.resultType.typedName === "List[Float]"
      x2.resultType.packageName === Some("scala.collection.immutable")

      val Some(x3) = x.members.find(_.decodedName == "quoted-1.2.3")
      x3.resultType.typedName === "String"

      val Some(x4) = x.members.find(_.decodedName == "genericValue")
      x4.resultType.packageName === Some("x7c1.salad.sample")
      x4.resultType.typedName ===
        "x7c1.salad.sample.GenericDisplayType[" +
          "String,x7c1.salad.sample.GenericDisplayType[Int,Float]]"

      val y = x4.resultType.members.head
      y.decodedName === "valueB"
      y.resultType.typedName ===
        "x7c1.salad.sample.GenericDisplayType[Int,Float]"

      val Some(x5) = x.members.find(_.decodedName == "values")
      val Some(y1) = x5.resultType.typeArguments.head.members.find(_.decodedName === "valueB")
      y1.resultType.typedName === "Int"
    }
    "inspect nested" in {
      val x = TypeStructureSample.nested
      x.packageName === Some("x7c1.salad.sample.x1.x2")
    }
    "inspect nested inner type" in {
      val x1 = TypeStructureSample.nestedInTrait
      x1.packageName === None
      x1.typedName === "x7c1.salad.sample.x1.x2.FooTrait#InnerTrait"

      val x2 = TypeStructureSample.nestedInObject
      x2.packageName === None
      x2.typedName === "x7c1.salad.sample.x1.x2.FooObject.InnerObject"
    }
  }
}

object TypeReflectionTest extends Specification {
  TypeReflection.getClass.getSimpleName should {
    "inspect a type structure by reflection" in {
      val x = TypeStructureSample.reflect
      x.typedName === classOf[SampleMemberType].getName

      val Some(x1) = x.members.find(_.decodedName == "intValue")
      x1.resultType.typedName === "scala.Int"

      val Some(x2) = x.members.find(_.decodedName == "listFloatValue")
      x2.resultType.typedName === "scala.List[scala.Float]"
      x2.resultType.packageName === Some("scala.collection.immutable")

      val Some(x3) = x.members.find(_.decodedName == "quoted-1.2.3")
      x3.resultType.typedName === "String"

      val Some(x4) = x.members.find(_.decodedName == "genericValue")
      x4.resultType.packageName === Some("x7c1.salad.sample")
      x4.resultType.typedName ===
        "x7c1.salad.sample.GenericDisplayType[" +
          "String,x7c1.salad.sample.GenericDisplayType[scala.Int,scala.Float]]"

      val y = x4.resultType.members.head
      y.decodedName === "valueB"
      y.resultType.typedName ===
        "x7c1.salad.sample.GenericDisplayType[scala.Int,scala.Float]"

      val Some(x5) = x.members.find(_.decodedName == "values")
      val Some(y1) = x5.resultType.typeArguments.head.members.find(_.decodedName === "valueB")
      y1.resultType.typedName === "scala.Int"
    }
    "inspect nested" in {
      val x = TypeStructureSample.reflectNestedPackage
      x.packageName === Some("x7c1.salad.sample.x1.x2")
    }
    "inspect nested inner type" in {
      val x1 = TypeStructureSample.reflectInTrait
      x1.packageName === None
      x1.typedName === "x7c1.salad.sample.x1.x2.FooTrait#InnerTrait"

      val x2 = TypeStructureSample.reflectInObject
      x2.packageName === None
      x2.typedName === "x7c1.salad.sample.x1.x2.FooObject.InnerObject"
    }
  }
}
