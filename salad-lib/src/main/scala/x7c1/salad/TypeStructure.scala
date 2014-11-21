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
            method.name -> method.typeSignatureIn(target)
        } collect {
          case (name, NullaryMethodType(resultType)) =>
            createField(
              fieldName = name.decodedName.toString,
              typeTree = buildFrom(resultType))
        }

      createType(
        typeName = target.toString,
        memberTrees = fields.toList )
    }
    def createType(typeName: String, memberTrees: List[Tree]) = {
      q"new ${typeOf[SaladType]}($typeName, $memberTrees)"
    }
    def createField(fieldName: String, typeTree: Tree) = {
      q"new ${typeOf[SaladField]}($fieldName, $typeTree)"
    }
    buildFrom(weakTypeOf[A])
  }
}

class SaladType(
  val typeName: String,
  val members: Seq[SaladField])

class SaladField(
  val fieldName: String,
  val resultType: SaladType)
