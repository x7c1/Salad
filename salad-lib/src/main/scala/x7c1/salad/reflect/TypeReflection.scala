package x7c1.salad.reflect

import x7c1.salad.{SaladField, SaladType}

import scala.reflect.runtime.universe._

object TypeReflection {
  def inspect[A: WeakTypeTag] = {
    val target = implicitly[WeakTypeTag[A]]

    def buildFrom(target: Type): SaladType = {
      val fields = target.members.view.
        filter(x => x.isMethod && x.isAbstract).
        filter(! _.owner.fullName.startsWith("scala")).
        map(_.asMethod).filter(_.paramLists.isEmpty).
        map{ method =>
          val resultType =  method.typeSignatureIn(target).resultType
          new SaladField(
            decodedName = method.name.decodedName.toString,
            resultType = buildFrom(resultType))
        }

      new SaladType(
        typedName = target.toString,
        members = fields.toList )
    }
    buildFrom(target.tpe)
  }
}
