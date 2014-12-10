package x7c1.salad.inspector

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object TypeExpander {
  def inspect[A]: TypeDigest = macro TypeExpanderImpl.inspect[A]
}

private object TypeExpanderImpl {
  def inspect[A: c.WeakTypeTag](c: blackbox.Context) = {
    import c.universe._

    def findPackage(symbol: Symbol) =
      if (symbol.isPackage) Some(symbol.fullName) else None

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
    def buildFrom(target: Type): Tree = {
      val fields = target.members.
        filter(x => x.isMethod && x.isAbstract).
        filter(! _.owner.fullName.startsWith("scala.")).
        map(_.asMethod).map {
          method =>
            method -> method.typeSignatureIn(target)
        } collect {
          case (method, NullaryMethodType(resultType)) =>
            createField(
              decodedName = method.name.decodedName.toString,
              rawTypeLabel = buildRawLabelFrom(method.returnType.resultType),
              typeTree = buildFrom(resultType))
        }

      val rawTypeArgs = {
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
      createType(
        packageName = findPackage(target.typeSymbol.owner),
        fullName = target.typeSymbol.fullName,
        typeArguments = target.typeArgs.map(x => buildFrom(x)),
        rawTypeArgs = rawTypeArgs,
        memberTrees = fields.toList )
    }
    def createType(
      packageName: Option[String],
      fullName: String,
      typeArguments: List[Tree],
      rawTypeArgs: Option[String],
      memberTrees: List[Tree]) = {

      q"new ${typeOf[TypeDigest]}($packageName, $fullName, $typeArguments, $rawTypeArgs, $memberTrees)"
    }

    def createField(decodedName: String, rawTypeLabel: String, typeTree: Tree) = {
      q"new ${typeOf[FieldSummary]}($decodedName, $rawTypeLabel, $typeTree)"
    }
    buildFrom(weakTypeOf[A])
  }

}
