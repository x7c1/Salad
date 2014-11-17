package x7c1.salad.sample

import org.specs2.mutable.Specification

object SaladSampleTest extends Specification{

  SaladSample.getClass.getSimpleName should {
    "create val2" in {
      val val2 = SaladSample.val2
      "hello" === val2
    }
    "create val4" in {
      val val4 = SaladSample.val4
      1.23 === val4
    }
    "create val5" in {
      val val5 = SaladSample.val5
      val5 === "foo5"
    }
    "create val7" in {
      val val7 = SaladSample.val7
      val7 === 777
    }
  }
}
