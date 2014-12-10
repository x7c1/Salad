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
          rawTypeLabel = buildRawLabelFrom(method.returnType.resultType),
          resultType = buildDigestFrom(resultType))
      }

    val rawTypeArgs = {
      val name = target.typeSymbol.fullName
      target.etaExpand.toString.split(name) match {
        /*
          e.g.
          target => trait Foo[S, Q <: S]
            x1 => [S, Q <: S]
            x2 => [S,Q]
         */
        case Array(x1, x2) => Some(x1)
        case _ => None
      }
    }
    new TypeDigest(
      packageName = findPackage(target.typeSymbol.owner),
      fullName = target.typeSymbol.fullName,
      typeArgs = target.typeArgs.map(buildDigestFrom),
      rawTypeArgsLabel = rawTypeArgs,
      members = fields.toList )
  }

  def buildRawLabelFrom(target: Type): String = {
    val name = target.typeSymbol match {
      case x if x.isParameter => x.name.decodedName.toString
      case x => x.fullName
    }
    val types = target.resultType.typeArgs.map(buildRawLabelFrom) match {
      case x if x.nonEmpty => x.mkString("[",",","]")
      case _ => ""
    }
    name + types
  }

  private def findPackage(symbol: Symbol) =
    if (symbol.isPackage) Some(symbol.fullName)
    else None
}
