package x7c1.salad

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object Salad {
  def Factory[A]: Any = macro SaladImpl.factory[A]
}

object SaladImpl {
  def factory[A: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val klass = weakTypeOf[A]
    val pairs = klass.members.
      filter(_.isMethod).
      filter(_.isAbstract).
      map(_.asMethod).map{ symbol =>
        symbol.name -> symbol.info.finalResultType
      }

    val inner = {
      val tuples = pairs zip (1 to pairs.size).map("$x%s" format _)
      val methods = tuples.map{ case ((term, resultType), tmp) =>
        DefDef(Modifiers(),
          term, List(), List(), TypeTree(), Ident(TermName(tmp)))
      }
      val values = tuples.map{ case ((term, resultType), tmp) =>
        ValDef(Modifiers(),
          TermName(tmp), TypeTree(), Ident(term))
      }
      q"{ ..$values; new $klass { ..$methods } }"
    }
    val parameters = pairs.map{ case (term, resultType) =>
      ValDef(Modifiers(), term, q"$resultType", EmptyTree)
    }
    val code = q"new { def apply(..$parameters) = $inner }"
    /*
    println(showRaw(code))
    println(showCode(code))
    */
    code
  }

}
