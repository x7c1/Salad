package x7c1.salad.inspector

class TypeDigest(
  /**
   * exists if enclosing symbol is package
   * otherwise none (e.g. if defined in object, trait, class)
   */
  val packageName: Option[String],
  val fullName: String,
  val typeArgs: Seq[TypeDigest],
  val rawTypeArgs: Option[String],
  val rawTypeFields: Seq[RawTypeField],
  val members: Seq[FieldSummary]){

  lazy val typedName = {
    def expand(x: TypeDigest): String = x.fullName + {
      if (x.typeArgs.isEmpty) ""
      else x.typeArgs.map(expand).mkString("[", ",", "]")
    }
    expand(this)
  }
}
