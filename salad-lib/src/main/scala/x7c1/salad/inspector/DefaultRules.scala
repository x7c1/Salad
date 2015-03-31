package x7c1.salad.inspector

object DefaultRules {

  val notBuiltIn: String => Boolean = { name =>
    !(Seq("scala", "java") exists name.startsWith)
  }
}
