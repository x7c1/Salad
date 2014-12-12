package x7c1.salad.inspector

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
      nameFilter = ! (_: String).startsWith("scala.")
    )
    factory.createFrom(weakTypeOf[A]) : c.Tree
  }
}
