type t =
  | Plus
  | Minus
  | Mult
  | Div
  | Mod
  | Carat
  | GT
  | LT
  | GE
  | LE
  | EQ
  | NE
  | Assign
  | Dot
  | Cat
  | Ellipsis
  | Colon
  | Dcolon
  | Semi
  | Comma
  | Hash
  | LBracket
  | RBracket
  | LBrace
  | RBrace
  | LParen
  | RParen
  | And
  | Break
  | Do
  | Else
  | Elseif
  | End
  | For
  | Function
  | Goto
  | If
  | In
  | Local
  | Not
  | NoArg
  | Or
  | Repeat
  | Return
  | Then
  | Until
  | While
  | True
  | False
  | Nil
  | Number of string
  | Ident of string
  | Str of string
  | EOF

let to_string = function
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Mult -> "Mult"
  | Div -> "Div"
  | Mod -> "Mod"
  | Carat -> "Carat"
  | GT -> "GT"
  | LT -> "LT"
  | GE -> "GE"
  | LE -> "LE"
  | EQ -> "EQ"
  | NE -> "NE"
  | Assign -> "Assign"
  | Dot -> "Dot"
  | Cat -> "Cat"
  | Ellipsis -> "Ellipsis"
  | Colon -> "Colon"
  | Dcolon -> "Dcolon"
  | Semi -> "Semi"
  | Comma -> "Comma"
  | Hash -> "Hash"
  | LBracket -> "LBracket"
  | RBracket -> "RBracket"
  | LBrace -> "LBrace"
  | RBrace -> "RBrace"
  | LParen -> "LParen"
  | RParen -> "RParen"
  | And -> "And"
  | Break -> "Break"
  | Do -> "Do"
  | Else -> "Else"
  | Elseif -> "Elseif"
  | End -> "End"
  | For -> "For"
  | Function -> "Function"
  | Goto -> "Goto"
  | If -> "If"
  | In -> "In"
  | Local -> "Local"
  | Not -> "Not"
  | NoArg -> "NoArg"
  | Or -> "Or"
  | Repeat -> "Repeat"
  | Return -> "Return"
  | Then -> "Then"
  | Until -> "Until"
  | While -> "While"
  | Num n -> "Num " ^ n
  | Ident s -> "Ident " ^ s
  | Bool b -> "Bool " ^ b
  | Str s -> "Str " ^ s
  | EOF -> "EOF"
