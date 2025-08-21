type token =
  (* unambiguous symbols *)
  | Plus
  | Minus
  | Mult
  | Div
  | Mod
  | Carat
  | Colon
  | Semicolon
  | Comma
  | Hash
  | LParen
  | RParen
  | LBrace
  | RBrace
  (* ambiguous symbols *)
  | LBrack
  | RBrack
  | Dash
  | Tilde
  | Assign
  | Dot
  | Concat
  | Dots
  | Eq
  | Ge
  | Geq
  | Le
  | Leq
  | Neq
  (* keywords *)
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
  (* literals, etc *)
  | Number of string
  | Str of string
  | Name of string

val tok_to_string: token -> string

val from_channel: in_channel -> token Seq.t
