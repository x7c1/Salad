package x7c1.salad.factory

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object TypeFactory {
  def apply[A]: Any = macro TypeFactoryImpl.factory[A]
}

object TypeFactoryImpl {
  def factory[A: c.WeakTypeTag](c: whitebox.Context) = {
    import c.universe._

    val klass = weakTypeOf[A]
    val pairs = klass.members.
      filter(_.isMethod).filter(_.isAbstract).map(_.asMethod).
      map {
      method => method.name -> method.typeSignatureIn(klass)
    } collect {
      case (term, NullaryMethodType(resultType)) => term -> resultType
    }

    val inner = {
      val tuples = pairs.zipWithIndex.map{ case ((term, _), index) =>
        term -> TermName("$x" + index)
      }
      val values = tuples.map{ case (term, tmp) =>
        q"val $tmp = $term"
      }
      val methods = tuples.map{ case (term, tmp) =>
        q"def $term = $tmp"
      }
      q"{ ..$values; new $klass { ..$methods } }"
    }
    val parameters = pairs.map{ case (term, resultType) =>
      q"$term:$resultType"
    }
    val code = q"new { def apply(..$parameters) = $inner }"
    /*
    println(showRaw(code))
    println(showCode(code))
    // */
    code
  }

}
