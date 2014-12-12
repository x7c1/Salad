package x7c1.salad.inspector

import scala.reflect.runtime.universe._

object TypeReflector {
  def inspect[A: WeakTypeTag]: TypeDigest = {
    val defaultFilter = ! (_: String).startsWith("scala.")
    new TypeReflector(defaultFilter).inspect[A]
  }
}

class TypeReflector(nameFilter: String => Boolean) {
  def inspect[A: WeakTypeTag]: TypeDigest = {
    val target = implicitly[WeakTypeTag[A]]
    new TypeDigestFactory(nameFilter) createDigestFrom target.tpe
  }
}
