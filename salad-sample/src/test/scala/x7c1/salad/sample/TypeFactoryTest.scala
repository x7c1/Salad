package x7c1.salad.sample

import org.specs2.mutable.Specification

object TypeFactoryTest extends Specification{

  TypeFactorySample.getClass.getSimpleName should {
    "create val2" in {
      val val2 = TypeFactorySample.val2
      "hello" === val2
    }
    "create val4" in {
      val val4 = TypeFactorySample.val4
      1.23 === val4
    }
    "create val5" in {
      val val5 = TypeFactorySample.val5
      val5 === "foo5"
    }
    "create val7" in {
      val val7 = TypeFactorySample.val7
      val7 === 777
    }
  }
}
