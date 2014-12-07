package x7c1.salad.inspector

class ObjectOutline (val methods: Seq[MethodSignature])

class MethodSignature(
  val decodedName: String,
  //  val argsList: Seq[Seq[MethodArgument]],
  val resultType: TypeDigest
)

//class MethodArgument(
//  val name: String,
//  val typeDigest: TypeDigest)
