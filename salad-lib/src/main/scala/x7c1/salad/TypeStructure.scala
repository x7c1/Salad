package x7c1.salad

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object TypeStructure {
  def inspect[A]: SaladType = macro TypeStructureImpl.inspect[A]
}

private object TypeStructureImpl {
  def inspect[A: c.WeakTypeTag](c: blackbox.Context) = {
    import c.universe._

    def findPackage(symbol: Symbol) =
      if (symbol.isPackage) Some(symbol.fullName) else None

    def buildFrom(target: Type): Tree = {
      val fields = target.members.
        filter(x => x.isMethod && x.isAbstract).
        filter(! _.owner.fullName.startsWith("scala")).
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
        typedName = target.toString,
        memberTrees = fields.toList )
    }
    def createType(
      packageName: Option[String],
      typedName: String,
      memberTrees: List[Tree]) = {

      q"new ${typeOf[SaladType]}($packageName, $typedName, $memberTrees)"
    }

    def createField(decodedName: String, typeTree: Tree) = {
      q"new ${typeOf[SaladField]}($decodedName, $typeTree)"
    }
    buildFrom(weakTypeOf[A])
  }
}

class SaladType(
  /**
   * exists if enclosing symbol is package
   * otherwise none (e.g. if defined in object, trait, class)
   */
  val packageName: Option[String],
  val typedName: String,
  val members: Seq[SaladField])

class SaladField(
  val decodedName: String,
  val resultType: SaladType)
