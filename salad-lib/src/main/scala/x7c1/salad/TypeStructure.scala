package x7c1.salad

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object TypeStructure {
  def inspect[A]: SaladType = macro TypeStructureImpl.inspect[A]
}

object TypeStructureImpl {
  def inspect[A: c.WeakTypeTag](c: blackbox.Context) = {
    import c.universe._

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
              decodeName = method.name.decodedName.toString,
              typeTree = buildFrom(resultType))
        }

      createType(
        typedName = target.toString,
        memberTrees = fields.toList )
    }
    def createType(typedName: String, memberTrees: List[Tree]) = {
      q"new ${typeOf[SaladType]}($typedName, $memberTrees)"
    }
    def createField(decodeName: String, typeTree: Tree) = {
      q"new ${typeOf[SaladField]}($decodeName, $typeTree)"
    }
    buildFrom(weakTypeOf[A])
  }
}

class SaladType(
  val typedName: String,
  val members: Seq[SaladField])

class SaladField(
  val decodedName: String,
  val resultType: SaladType)
