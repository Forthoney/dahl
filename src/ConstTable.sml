structure ConstTable = IDMapFn (
  HashTableFn (
    structure Key = Constant.Hashable
    val maxLoad = 0.7
    val slopFactor = 2.0
  )
)
