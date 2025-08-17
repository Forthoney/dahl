type token =
  | Dash
  | Tilde
  | LBrack
  | RBrack
  | Assign
  | And
  | Break
  | Do
  | Else
  | Elseif
  | End
  | False
  | For
  | Function
  | If
  | In
  | Local
  | Nil
  | Not
  | Or
  | Repeat
  | Return
  | Then
  | True
  | Until
  | While
  | Concat
  | Dots
  | Eq
  | Ge
  | Geq
  | Le
  | Leq
  | Neq
  | Number of string
  | Name of string
  | Str of string

val lex : in_channel -> token Seq.t
