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
  }
}
