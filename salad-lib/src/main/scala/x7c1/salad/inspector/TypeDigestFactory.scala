package x7c1.salad.inspector

import scala.reflect.runtime.universe._

trait TypeDigestFactory {

  def nameFilter: String => Boolean

  def buildDigestFrom(target: Type): TypeDigest = {
    val fields = target.members.view.
      filter(x => nameFilter(x.owner.fullName)).
      filter(x => x.isMethod && x.isAbstract).
      map(_.asMethod).filter(_.paramLists.isEmpty).
      map{ method =>
      val resultType =  method.typeSignatureIn(target).resultType
      new FieldSummary(
        decodedName = method.name.decodedName.toString,
        resultType = buildDigestFrom(resultType))
    }
    new TypeDigest(
      packageName = findPackage(target.typeSymbol.owner),
      fullName = target.typeSymbol.fullName,
      typeArguments = target.typeArgs.map(buildDigestFrom),
      members = fields.toList )
  }
  private def findPackage(symbol: Symbol) =
    if (symbol.isPackage) Some(symbol.fullName)
    else None
}
