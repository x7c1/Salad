package x7c1.salad.inspector

class MethodSignature(
  val decodedName: String,
  val argumentsList: Seq[Seq[MethodArgument]],
  val resultType: TypeDigest
)

class MethodArgument(
  val name: String,
  val typeDigest: TypeDigest
)
