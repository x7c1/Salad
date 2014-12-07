package x7c1.salad.inspector
import scala.reflect.runtime.universe._

class ObjectReflector(
  override val nameFilter: String => Boolean) extends TypeDigestFactory {

  def inspect[A: WeakTypeTag](scrutinee: A): ObjectOutline = {
    val target = implicitly[WeakTypeTag[A]]

    val methods = target.tpe.members.view.
      filter(x => nameFilter(x.owner.fullName)).
      filter(x => x.isMethod && x.isPublic).
      map(_.asMethod).
      map{ method =>
        println(method.paramLists)
        val resultType =  method.typeSignatureIn(target.tpe).resultType
        new MethodSignature(
          method.name.decodedName.toString,
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
