package x7c1.salad.inspector

import scala.reflect.macros.blackbox

class TypeDigestTreeFactory[C <: blackbox.Context](
  val context: C,
  val nameFilter: String => Boolean ){

  import context.universe._

  def createFrom(target: Type): Tree = {
    val fields = target.members.view.
      filter(x => x.isMethod && x.isAbstract).
      filter(x => nameFilter(x.owner.fullName)).
      map(_.asMethod).filter(_.paramLists.isEmpty).
      map{ method =>
        val resultOf = method.typeSignatureIn(_: Type).resultType
        createField(
          decodedName = method.name.decodedName.toString,
          rawTypeLabel = buildRawLabelFrom(resultOf(target.etaExpand)),
          typeTree = createFrom(resultOf(target)))
      }

    createType(
      packageName = findPackage(target.typeSymbol.owner),
      fullName = target.typeSymbol.fullName,
      typeArgs = target.typeArgs.map(x => createFrom(x)),
      typeArgsLabel = typeArgsRawLabelOf(target),
      memberTrees = fields.toList )
  }

  private def findPackage(symbol: Symbol) =
    if (symbol.isPackage) Some(symbol.fullName) else None

  private def buildRawLabelFrom(target: Type): String = {
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

  private def typeArgsRawLabelOf(target: Type): Option[String] = {
    val pattern = s"""\\((.*)${target.typeSymbol.fullName}.*""".r
    target.erasure.etaExpand.toString match {
      /*
        e.g.
        target => trait Foo[S, Q <: S]
          x => [S, Q <: S]
       */
      case pattern(x) => Some(x)
      case _ => None
    }
  }

  private def createType(
    packageName: Option[String],
    fullName: String,
    typeArgs: List[Tree],
    typeArgsLabel: Option[String],
    memberTrees: List[Tree]) = {

    q"""new ${typeOf[TypeDigest]}(
      $packageName, $fullName, $typeArgs, $typeArgsLabel, $memberTrees)"""
  }

  private def createField(decodedName: String, rawTypeLabel: String, typeTree: Tree) = {
    q"new ${typeOf[FieldSummary]}($decodedName, $rawTypeLabel, $typeTree)"
  }
}
