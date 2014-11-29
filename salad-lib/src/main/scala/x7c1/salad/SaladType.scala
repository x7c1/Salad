package x7c1.salad

class SaladType(
  /**
   * exists if enclosing symbol is package
   * otherwise none (e.g. if defined in object, trait, class)
   */
  val packageName: Option[String],
  val typedName: String,
  val typeArguments: Seq[SaladType],
  val members: Seq[SaladField])
