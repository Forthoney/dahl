structure Constant =
struct
  datatype t = STR of string | NUM of real

  structure Hashable : HASHABLE =
  struct
    type t = t
    fun eq (STR s, STR s') = s = s'
      | eq (NUM n, NUM n') = Real.== (n, n')
      | eq _ = false

    fun hash (STR s) = FNV.String.hash s
      | hash (NUM n) = Word64.fromInt (Real.round n)
  end
end

