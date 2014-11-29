package x7c1.salad.sample
import org.specs2.mutable.Specification
import x7c1.salad.inspector.{TypeReflector, TypeExpander}

trait CommonTests {
  this: Specification =>
  def types: SampleTypes

  "inspect package" in {
    val x = types.nestedPackage
    x.packageName === Some("x7c1.salad.sample.x1.x2")
  }

  "inspect nested inner type" in {
    val x1 = types.nestedInTrait
    x1.packageName === None
    x1.typedName === "x7c1.salad.sample.x1.x2.FooTrait#InnerTrait"

    val x2 = types.nestedInObject
    x2.packageName === None
    x2.typedName === "x7c1.salad.sample.x1.x2.FooObject.InnerObject"
  }

  "inspect quoted type name" in {
    val x1 = types.sampleType
    val x2 = x1.members.find(_.decodedName == "quoted-1.2.3")
    x2.isDefined === true
  }

  "inspect type defined in Predef" in {
    val x1 = types.sampleType
    val Some(x2) = x1.members.find(_.decodedName == "stringValue")
    x2.resultType.typedName === "String"
  }
}

object TypeStructureTest extends Specification with CommonTests {
  override def types = TypesByMacro

  TypeExpander.getClass.getSimpleName should {
    "inspect a type structure by macro" in {
      val x = types.sampleType
      x.typedName === classOf[SampleType].getName

      val Some(x1) = x.members.find(_.decodedName == "intValue")
      x1.resultType.typedName === "Int"

      val Some(x2) = x.members.find(_.decodedName == "listFloatValue")
      x2.resultType.typedName === "List[Float]"
      x2.resultType.packageName === Some("scala.collection.immutable")

      val Some(x4) = x.members.find(_.decodedName == "genericValue")
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
  }
}

object TypeReflectionTest extends Specification with CommonTests{
  override def types = TypesByReflection

  TypeReflector.getClass.getSimpleName should {
    "inspect a type structure by reflection" in {
      val x = types.sampleType
      x.typedName === classOf[SampleType].getName

      val Some(x1) = x.members.find(_.decodedName == "intValue")
      x1.resultType.typedName === "scala.Int"

      val Some(x2) = x.members.find(_.decodedName == "listFloatValue")
      x2.resultType.typedName === "scala.List[scala.Float]"
      x2.resultType.packageName === Some("scala.collection.immutable")

      val Some(x4) = x.members.find(_.decodedName == "genericValue")
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
  }
}
