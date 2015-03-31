package x7c1.salad.inspector.reflect

import x7c1.salad.inspector.TypeDigest

import scala.reflect.runtime.universe._

object TypeReflector {
  def inspect[A: WeakTypeTag]: TypeDigest = {
    val defaultFilter = (x: String) =>
      !(x startsWith "scala") && !(x startsWith "java")

    new TypeReflector(defaultFilter).inspect[A]
  }
}

class TypeReflector(nameFilter: String => Boolean) {
  def inspect[A: WeakTypeTag]: TypeDigest = {
    val target = implicitly[WeakTypeTag[A]]
    new TypeDigestFactory(nameFilter) createDigestFrom target.tpe
  }
}
