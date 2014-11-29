package x7c1.salad.inspector

import scala.reflect.runtime.universe._

object TypeReflector {
  def inspect[A: WeakTypeTag]: SaladType = {
    val target = implicitly[WeakTypeTag[A]]

    def findPackage(symbol: Symbol) =
      if (symbol.isPackage) Some(symbol.fullName) else None

    def buildFrom(target: Type): SaladType = {
      val fields = target.members.view.
        filter(x => x.isMethod && x.isAbstract).
        filter(! _.owner.fullName.startsWith("scala.")).
        map(_.asMethod).filter(_.paramLists.isEmpty).
        map{ method =>
          val resultType =  method.typeSignatureIn(target).resultType
          new SaladField(
            decodedName = method.name.decodedName.toString,
            resultType = buildFrom(resultType))
        }

      new SaladType(
        packageName = findPackage(target.typeSymbol.owner),
        typedName = target.toString,
        typeArguments = target.typeArgs.map(buildFrom),
        members = fields.toList )
    }
    buildFrom(target.tpe)
  }
}
