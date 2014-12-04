package x7c1.salad.inspector

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object TypeExpander {
  def inspect[A]: SaladType = macro TypeExpanderImpl.inspect[A]
}

private object TypeExpanderImpl {
  def inspect[A: c.WeakTypeTag](c: blackbox.Context) = {
    import c.universe._

    def findPackage(symbol: Symbol) =
      if (symbol.isPackage) Some(symbol.fullName) else None

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
              typeTree = buildFrom(resultType))
        }

      createType(
        packageName = findPackage(target.typeSymbol.owner),
        fullName = target.typeSymbol.fullName,
        typeArguments = target.typeArgs.map(x => buildFrom(x)),
        memberTrees = fields.toList )
    }
    def createType(
      packageName: Option[String],
      fullName: String,
      typeArguments: List[Tree],
      memberTrees: List[Tree]) = {

      q"new ${typeOf[SaladType]}($packageName, $fullName, $typeArguments, $memberTrees)"
    }

    def createField(decodedName: String, typeTree: Tree) = {
      q"new ${typeOf[SaladField]}($decodedName, $typeTree)"
    }
    buildFrom(weakTypeOf[A])
  }
}
