package x7c1.salad.inspector

import scala.reflect.runtime.universe._

object TypeReflector {
  def inspect[A: WeakTypeTag]: TypeDigest = {
    val defaultFilter = ! (_: String).startsWith("scala.")
    new TypeReflector(defaultFilter).inspect[A]
  }
}
class TypeReflector(nameFilter: String => Boolean){
  def inspect[A: WeakTypeTag]: TypeDigest = {
    val target = implicitly[WeakTypeTag[A]]

    def findPackage(symbol: Symbol) =
      if (symbol.isPackage) Some(symbol.fullName) else None

    def buildFrom(target: Type): TypeDigest = {
      val fields = target.members.view.
        filter(x => nameFilter(x.owner.fullName)).
        filter(x => x.isMethod && x.isAbstract).
        map(_.asMethod).filter(_.paramLists.isEmpty).
        map{ method =>
          val resultType =  method.typeSignatureIn(target).resultType
          new FieldSummary(
            decodedName = method.name.decodedName.toString,
            resultType = buildFrom(resultType))
        }

      new TypeDigest(
        packageName = findPackage(target.typeSymbol.owner),
        fullName = target.typeSymbol.fullName,
        typeArguments = target.typeArgs.map(buildFrom),
        members = fields.toList )
    }
    buildFrom(target.tpe)
  }
}
