package x7c1.salad.sample

import org.scalatest.{Matchers, FlatSpecLike}

import x7c1.salad.inspector.FieldSummary

trait TraitCommonTests {
  this: FlatSpecLike with Matchers =>

  def types: SampleTypes

  behavior of types.getClass.getName

  it can "inspect package" in {
    val x = types.nestedPackage
    x.packageName shouldBe Some("x7c1.salad.sample.x1.x2")
  }

  it can "inspect nested inner type" in {
    val x1 = types.nestedInTrait
    x1.packageName shouldBe None
    x1.typedName shouldBe "x7c1.salad.sample.x1.x2.FooTrait.InTrait"

    val x2 = types.nestedInObject
    x2.packageName shouldBe None
    x2.typedName shouldBe "x7c1.salad.sample.x1.x2.FooObject.InObject"
  }

  it can "inspect quoted field name" in {
    val x1 = types.sampleType
    val x2 = x1.members.find(_.decodedName == "quoted-1.2.3")
    x2.isDefined shouldBe true
  }

  it can "inspect type defined in Predef" in {
    val x1 = types.sampleType
    val Some(x2) = x1.members.find(_.decodedName == "stringValue")
    x2.resultType.typedName shouldBe "java.lang.String"
  }

  it can "inspect type structure" in {
    val x = types.sampleType
    x.typedName shouldBe classOf[SampleType].getName

    val Some(x1) = x.members.find(_.decodedName == "intValue")
    x1.resultType.typedName shouldBe "scala.Int"

    val Some(x2) = x.members.find(_.decodedName == "listFloatValue")
    x2.resultType.typedName shouldBe "scala.collection.immutable.List[scala.Float]"
    x2.resultType.packageName shouldBe Some("scala.collection.immutable")

    val Some(x4) = x.members.find(_.decodedName == "genericValue")
    x4.resultType.typedName shouldBe
      "x7c1.salad.sample.GenericType[" +
        "java.lang.String,x7c1.salad.sample.GenericType[scala.Int,scala.Float]]"

    val y = x4.resultType.members.head
    y.decodedName shouldBe "valueB"
    y.resultType.typedName shouldBe
      "x7c1.salad.sample.GenericType[scala.Int,scala.Float]"

    val Some(x5) = x.members.find(_.decodedName == "values")
    val Some(y1) = x5.resultType.typeArgs.head.members.find(_.decodedName == "valueB")
    y1.resultType.typedName shouldBe "scala.Int"
  }

  object typeLabel {
    class Inner(name: String){
      def in(summary: FieldSummary) =
        summary.resultType.members.find(_.decodedName == name).map(_.resultTypeRawLabel)
    }
    def of(name: String) = new Inner(name)
  }

  it can "inspect raw type parameters" in {
    val x = types.sampleType2
    x.typeArgsRawLabel shouldBe None

    val Some(x4) = x.members.find(_.decodedName == "genericValue")
    x4.resultType.typeArgsRawLabel shouldBe Some("[S, Q <: S]")
    (typeLabel of "genericFunction" in x4) shouldBe Some("scala.Function1[S,Q]")

    val Some(x7) = x4.resultType.members.find(_.decodedName == "genericSeq")
    x7.resultType.typeArgsRawLabel shouldBe Some("[X, Y]")
    (typeLabel of "valueB" in x7) shouldBe Some("Y")
  }

  it can "inspect raw type parameters of scala.*" in {
    val x = types.sampleType2
    val Some(x4) = x.members.find(_.decodedName == "genericValue")
    (typeLabel of "genericSeq" in x4) shouldBe
      Some("x7c1.salad.sample.GenericType[S,scala.collection.Seq[Q]]")
  }

  it can "inspect raw type parameters of merged structure" in {
    val x = types.mergedType
    val Some(x2) = x.members.find(_.decodedName == "value")
    x2.resultType.typeArgsRawLabel shouldBe Some("[A]")
    x2.resultType.typedName shouldBe "x7c1.salad.sample.InnerMergedType[java.lang.String]"

    (typeLabel of "foo" in x2) shouldBe Some("A")
    (typeLabel of "bar" in x2) shouldBe Some("scala.Int")
    (typeLabel of "baz" in x2) shouldBe Some("scala.Long")
  }
}

class TraitMacroTest extends FlatSpecLike with Matchers with TraitCommonTests {
  override def types = TypesByMacro
}

class TraitReflectionTest extends FlatSpecLike with Matchers with TraitCommonTests{
  override def types = TypesByReflection
}
