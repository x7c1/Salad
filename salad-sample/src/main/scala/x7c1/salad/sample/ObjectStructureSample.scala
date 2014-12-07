package x7c1.salad.sample

import x7c1.salad.inspector.ObjectReflector

object ObjectInspectorByReflection {
  def sample = {
    ObjectReflector.inspect(SampleObject)
  }
}

object SampleObject {

  def sampleMethod(sampleArg: SampleType): Seq[SampleType] = ???

  private def privateMethod(foo: Int) = ???
}
