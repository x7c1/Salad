package x7c1.salad.sample
import org.specs2.mutable.Specification

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
    x1.typedName === "x7c1.salad.sample.x1.x2.FooTrait.InTrait"

    val x2 = types.nestedInObject
    x2.packageName === None
    x2.typedName === "x7c1.salad.sample.x1.x2.FooObject.InObject"
  }

  "inspect quoted field name" in {
    val x1 = types.sampleType
    val x2 = x1.members.find(_.decodedName == "quoted-1.2.3")
    x2.isDefined === true
  }

  "inspect type defined in Predef" in {
    val x1 = types.sampleType
    val Some(x2) = x1.members.find(_.decodedName == "stringValue")
    x2.resultType.typedName === "java.lang.String"
  }

  "inspect type structure" in {
    val x = types.sampleType
    x.typedName === classOf[SampleType].getName

    val Some(x1) = x.members.find(_.decodedName == "intValue")
    x1.resultType.typedName === "scala.Int"

    val Some(x2) = x.members.find(_.decodedName == "listFloatValue")
    x2.resultType.typedName === "scala.collection.immutable.List[scala.Float]"
    x2.resultType.packageName === Some("scala.collection.immutable")

    val Some(x4) = x.members.find(_.decodedName == "genericValue")
    x4.resultType.typedName ===
      "x7c1.salad.sample.GenericDisplayType[" +
        "java.lang.String,x7c1.salad.sample.GenericDisplayType[scala.Int,scala.Float]]"

    val y = x4.resultType.members.head
    y.decodedName === "valueB"
    y.resultType.typedName ===
      "x7c1.salad.sample.GenericDisplayType[scala.Int,scala.Float]"

    val Some(x5) = x.members.find(_.decodedName == "values")
    val Some(y1) = x5.resultType.typeArgs.head.members.find(_.decodedName === "valueB")
    y1.resultType.typedName === "scala.Int"
  }
}

object TypeStructureTest extends Specification with CommonTests {
  override def types = TypesByMacro
}

object TypeReflectionTest extends Specification with CommonTests{
  override def types = TypesByReflection

  "inspect raw type parameters" in {
    val x = types.sampleType2
    x.rawTypeArgs === None

    val Some(x4) = x.members.find(_.decodedName == "genericValue")
    x4.resultType.rawTypeArgs === Some("[S, Q <: S]")
    x4.resultType.rawTypeFields.
      find(_.decodedName == "genericFunction").map(_.rawTypeLabel) ===
        Some("S => Q")

    x4.resultType.rawTypeFields.
      find(_.decodedName == "genericSeq").map(_.rawTypeLabel) ===
        Some("x7c1.salad.sample.GenericDisplayType[S,scala.Seq[Q]]")

    val Some(x7) = x4.resultType.members.find(_.decodedName == "genericSeq")
    x7.resultType.rawTypeArgs === Some("[X, Y]")
    x7.resultType.rawTypeFields.
      find(_.decodedName == "valueB").map(_.rawTypeLabel) ===
        Some("Y")
  }

}
