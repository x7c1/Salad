package x7c1.salad.inspector.reflect

import x7c1.salad.inspector.DefaultRules.notBuiltIn
import x7c1.salad.inspector.TypeDigest

import scala.reflect.runtime.universe._

object TypeReflector {
  def inspect[A: WeakTypeTag]: TypeDigest = {
    new TypeReflector(notBuiltIn).inspect[A]
  }
}

class TypeReflector(nameFilter: String => Boolean) {
  def inspect[A: WeakTypeTag]: TypeDigest = {
    val target = implicitly[WeakTypeTag[A]]
    new TypeDigestFactory(nameFilter) createDigestFrom target.tpe
  }
}
