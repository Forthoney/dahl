signature SEXP =
sig
  datatype t =
    Atom of string
  | List of t list

  val symbol: string -> t
  val namedStruct: string * t list -> t
  val toString: t -> string
end
