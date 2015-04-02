package x7c1.salad.inspector.macros

import x7c1.salad.inspector.{FieldSummary, TypeDigest}

import scala.reflect.macros.blackbox

class TypeDigestTreeFactory[C <: blackbox.Context](
  val context: C, nameFilter: String => Boolean ){

  import context.universe._

  def createFrom(target: Type): Tree = {

    val base = target.members.view.
      filter(x => nameFilter(x.owner.fullName)).
      filter(_.isMethod)

    def classFields = base.
      filter(_.isPublic).map(_.asMethod).
      filter(_.isGetter)

    def traitFields = base.
      filter(_.isAbstract).map(_.asMethod).
      filter(_.paramLists.isEmpty)

    val isTrait = {
      val constructor = target.members.find(_.isConstructor).map(_.asMethod)
      val arguments = constructor.toSeq.map(_.paramLists.flatten).flatten
      arguments.isEmpty
    }
    val fields = { if(isTrait) traitFields else classFields } map {
      method =>
        val resultOf = method.typeSignatureIn(_: Type).resultType
        createField(
          decodedName = method.name.decodedName.toString,
          rawTypeLabel = buildRawLabelFrom(resultOf(target.etaExpand)),
          typeTree = createFrom(resultOf(target)))
    }
    createType(
      packageName = findPackage(target.typeSymbol.owner),
      fullName = target.typeSymbol.fullName,
      typeArgs = target.typeArgs map createFrom,
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
