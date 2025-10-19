signature LEXER =
sig
  datatype token =
    AND
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
  | NAME of string
  | INT of int
  | FLOAT of real
  | STRING of string
  (* arithmetic *)
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | MODULO
  | CARET
  (* relational *)
  | EQEQ
  | NEQ
  | LT
  | GT
  | LTE
  | GTE
  | CONCAT
  | LENGTH
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | LBRACE
  | RBRACE
  | ASSIGN
  | COLON
  | SEMICOLON
  | COMMA
  | DOT
  | VARARG

  (* punctuation helpers *)
  val isAssign: token -> bool
  val isComma: token -> bool
  val isDot: token -> bool
  val isColon: token -> bool
  val isSemicolon: token -> bool
  
  val toString: token -> string
  val scan: (token, TextIO.StreamIO.instream) StringCvt.reader
end
