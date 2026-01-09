structure Value =
struct
  datatype t =
    Nil
  | Boolean of bool
  | String of string
  | Number of real

  fun toString v =
    case v of
      Nil => "nil"
    | Boolean b => Bool.toString b
    | String s => "\"" ^ String.toString s ^ "\""
    | Number n => Real.fmt (StringCvt.GEN (SOME 4)) n
end
