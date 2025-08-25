type token =
  (* keywords *)
  | AND
  | BREAK
  | DO
  | ELSE
  | ELSEIF
  | END
  | FALSE
  | FOR
  | FUNCTION
  | IF
  | IN
  | LOCAL
  | NIL
  | NOT
  | OR
  | REPEAT
  | RETURN
  | THEN
  | TRUE
  | UNTIL
  | WHILE
  | IDENT of string
  | NUMBER of string
  | STRING of string
  (* operators and punctuation *)
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | MODULO
  | CARET
  | HASH
  | ASSIGN
  | EQ
  | NEQ
  | LT
  | LTE
  | GT
  | GTE
  | DOT
  | CONCAT
  | ELLIPSIS
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | SEMICOLON
  | COLON
  | COMMA
  | EOF

let to_string = function
  | AND -> "AND"
  | BREAK -> "BREAK"
  | DO -> "DO"
  | ELSE -> "ELSE"
  | ELSEIF -> "ELSEIF"
  | END -> "END"
  | FALSE -> "FALSE"
  | FOR -> "FOR"
  | FUNCTION -> "FUNCTION"
  | IF -> "IF"
  | IN -> "IN"
  | LOCAL -> "LOCAL"
  | NIL -> "NIL"
  | NOT -> "NOT"
  | OR -> "OR"
  | REPEAT -> "REPEAT"
  | RETURN -> "RETURN"
  | THEN -> "THEN"
  | TRUE -> "TRUE"
  | UNTIL -> "UNTIL"
  | WHILE -> "WHILE"
  | IDENT s -> "IDENT " ^ s
  | NUMBER n -> "NUMBER " ^ n
  | STRING s -> "STRING " ^ s
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | STAR -> "STAR"
  | SLASH -> "SLASH"
  | MODULO -> "MODULO"
  | CARET -> "CARET"
  | HASH -> "HASH"
  | ASSIGN -> "ASSIGN"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | LT -> "LT"
  | LTE -> "LTE"
  | GT -> "GT"
  | GTE -> "GTE"
  | DOT -> "DOT"
  | CONCAT -> "CONCAT"
  | ELLIPSIS -> "ELLIPSIS"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | SEMICOLON -> "SEMICOLON"
  | COLON -> "COLON"
  | COMMA -> "COMMA"
  | EOF -> "EOF"
