package x7c1.salad.sample.factory

import org.scalatest.{FlatSpecLike, Matchers}

class TypeFactoryTest extends FlatSpecLike with Matchers {

  behavior of TypeFactorySample.getClass.getSimpleName

  it can "create val2" in {
    val val2 = TypeFactorySample.val2
    "hello" shouldBe val2
  }
  it can "create val4" in {
    val val4 = TypeFactorySample.val4
    1.23 shouldBe val4
  }
  it can "create val5" in {
    val val5 = TypeFactorySample.val5
    val5 shouldBe "foo5"
  }
  it can "create val7" in {
    val val7 = TypeFactorySample.val7
    val7 shouldBe 777
  }

}
