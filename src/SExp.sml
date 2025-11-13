structure SExp : SEXP =
struct
  datatype t =
    Atom of string
  | List of t list

  fun namedStruct (name, props) =
    List (Atom name :: props)

  fun symbol sym =
    Atom ("'" ^ sym)

  fun toString (Atom s) = s
    | toString (List xs) = "(" ^ String.concatWith " " (map toString xs) ^ ")"
end
