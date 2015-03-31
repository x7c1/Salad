package x7c1.salad.inspector.macros

import x7c1.salad.inspector.DefaultRules.notBuiltIn
import x7c1.salad.inspector.TypeDigest

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object TypeExpander {
  def inspect[A]: TypeDigest = macro TypeExpanderImpl.inspect[A]
}

private object TypeExpanderImpl {
  def inspect[A: c.WeakTypeTag](c: blackbox.Context) = {
    import c.universe._

    val factory = new TypeDigestTreeFactory[c.type](
      context = c,
      nameFilter = notBuiltIn
    )
    factory.createFrom(weakTypeOf[A]) : c.Tree
  }
}
