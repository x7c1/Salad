package x7c1.salad.debug

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object prints {
  def apply[A](x: => A): Unit = macro printsImpl.impl[A]
}

private object printsImpl {
  def impl[A: c.WeakTypeTag](c: blackbox.Context)(x: c.Expr[A]) = {
    import c.universe._
    val source = showCode(x.tree)
    val line = s"L${x.tree.pos.line}: $source => "
    q"""println($line + ${x.tree})"""
  }
}
