package x7c1.salad.inspector

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object sprint {
  def apply[A](x: => A): Unit = macro sprintImpl.impl[A]
}

private object sprintImpl {
  def impl[A: c.WeakTypeTag](c: blackbox.Context)(x: c.Expr[A]) = {
    import c.universe._
    val source = showCode(x.tree)
    val line = s"L${x.tree.pos.line}: $source => "
    q"""println($line + ${x.tree})"""
  }
}
