structure Value =
struct
  datatype t =
    Nil
  | Boolean of bool
  | String of string
  | Number of real

  fun eq (Number l, Number r) = Real.== (l, r)
    | eq (Boolean l, Boolean r) = l = r
    | eq (String l, String r) = l = r
    | eq _ = false

  fun toString v =
    case v of
      Nil => "nil"
    | Boolean b => Bool.toString b
    | String s => "\"" ^ String.toString s ^ "\""
    | Number n => Real.fmt (StringCvt.GEN (SOME 4)) n

  structure Hashable : HASHABLE =
  struct
    type t = t

    val eq = eq

    fun hash (String s) = FNV.String.hash s
      | hash (Number n) = Word64.fromInt (Real.round n)
      | hash (Boolean false) = (0w0 : Word64.word)
      | hash (Boolean true) = (0w1 : Word64.word)
      | hash Nil = (0w2 : Word64.word)
  end
end
