package x7c1.salad.inspector

class SaladType(
  /**
   * exists if enclosing symbol is package
   * otherwise none (e.g. if defined in object, trait, class)
   */
  val packageName: Option[String],
  val fullName: String,
  val typeArguments: Seq[SaladType],
  val members: Seq[SaladField]){

  lazy val typedName = {
    def expand(x: SaladType): String = x.fullName + {
      if (x.typeArguments.isEmpty) ""
      else x.typeArguments.map(expand).mkString("[", ",", "]")
    }
    expand(this)
  }
}
