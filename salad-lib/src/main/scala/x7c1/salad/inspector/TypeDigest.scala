package x7c1.salad.inspector

class TypeDigest(
  /**
   * exists if enclosing symbol is package
   * otherwise none (e.g. if defined in object, trait, class)
   */
  val packageName: Option[String],
  val fullName: String,
  val typeArguments: Seq[TypeDigest],
  val members: Seq[FieldSummary]){

  lazy val typedName = {
    def expand(x: TypeDigest): String = x.fullName + {
      if (x.typeArguments.isEmpty) ""
      else x.typeArguments.map(expand).mkString("[", ",", "]")
    }
    expand(this)
  }
}
