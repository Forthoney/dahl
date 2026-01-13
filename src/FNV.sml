structure FNV =
struct
  structure String : HASHABLE =
  struct
    type t = string
    val eq = op=

    val offsetBasis : Word64.word = 0wxcbf29ce484222325
    val prime : Word64.word = 0wx00000100000001B3
    
    fun hashSingle bh = Word64.xorb bh * prime
    fun hashChar (c, h) = hashSingle (Word64.fromInt (Char.ord c), h)

    fun hash s = Word64.fromLarge (Vector.foldl hashChar offsetBasis s)
  end
end
