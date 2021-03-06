package x7c1.salad.inspector.reflect

import x7c1.salad.inspector.DefaultRules.notBuiltIn
import x7c1.salad.inspector.{MethodArgument, MethodSignature, ObjectOutline}

import scala.reflect.runtime.universe._

class ObjectReflector(nameFilter: String => Boolean){

  private val factory = new TypeDigestFactory(nameFilter)

  def inspect[A: WeakTypeTag](scrutinee: A): ObjectOutline = {
    val target = implicitly[WeakTypeTag[A]]

    def toMethodArgument(symbol: Symbol) = {
      new MethodArgument(
        symbol.name.toString,
        factory createDigestFrom symbol.typeSignature)
    }
    val methods = target.tpe.members.view.
      filter(x => nameFilter(x.owner.fullName)).
      filter(x => x.isMethod && x.isPublic).
      map(_.asMethod).
      map{ method =>
        val resultType =  method.typeSignatureIn(target.tpe).resultType
        new MethodSignature(
          method.name.decodedName.toString,
          method.paramLists.map(_ map toMethodArgument),
          factory createDigestFrom resultType)
      }

    new ObjectOutline(
      target.tpe.typeSymbol.fullName,
      target.tpe.typeSymbol.name.decodedName.toString,
      methods.toList
    )
  }
}

object ObjectReflector {
  def inspect[A: WeakTypeTag](scrutinee: A): ObjectOutline = {
    new ObjectReflector(notBuiltIn) inspect scrutinee
  }
}
