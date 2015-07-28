package x7c1.salad.parameter

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object Agglomerator {
  def define[A](f: A): Any = macro AgglomeratorImpl.define[A]
}

private object AgglomeratorImpl {
  def define[A: c.WeakTypeTag](c: whitebox.Context)(f: c.Expr[A]) = {
    import c.universe._

    val args = f.tree.children.filter(_.isDef)
    val names = args.map(_.symbol.asTerm.name)

    val fields = args.map{ x =>
      q"def ${x.symbol.asTerm.name}: ${x.symbol.typeSignature}"
    }
    val A = TypeName(c.freshName())
    val arg = TermName(c.freshName())
    val access: TermName => Tree = key => q"$arg.$key"

    q"""
      new {
        type $A = { ..$fields }
        def apply($arg: $A) = $f(..${names map access})
        def apply(..$args) = $f(..$names)
      }
    """
  }
}
