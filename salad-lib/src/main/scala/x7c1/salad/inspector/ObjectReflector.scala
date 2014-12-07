package x7c1.salad.inspector
import scala.reflect.runtime.universe._

class ObjectReflector(
  override val nameFilter: String => Boolean) extends TypeDigestFactory {

  def inspect[A: WeakTypeTag](scrutinee: A): ObjectOutline = {
    val target = implicitly[WeakTypeTag[A]]

    def toMethodArgument(symbol: Symbol) = {
      new MethodArgument(
        symbol.name.toString,
        buildDigestFrom(symbol.typeSignature))
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
          buildDigestFrom(resultType))
      }

    new ObjectOutline(methods.toList)
  }
}

object ObjectReflector {
  def inspect[A: WeakTypeTag](scrutinee: A): ObjectOutline = {
    val defaultFilter = (x: String) =>
      !x.startsWith("scala.") &&
      !x.startsWith("java.")

    new ObjectReflector(defaultFilter).inspect(scrutinee)
  }
}