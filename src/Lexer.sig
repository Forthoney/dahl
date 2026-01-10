signature LEXER =
sig
  exception Unterminated of {inner : string, expected : string, lineno : int}
  exception Invalid of {found : string, lineno : int}

  datatype token =
    COMMENT of string
  | ADD | SUB | MUL | DIV | MOD | POW
  | LEN
  | L_PAREN | R_PAREN | L_BRACK | R_BRACK | L_BRACE | R_BRACE
  | COLON | SEMICOLON | COMMA
  | DOT | CONCAT | VARARG
  | EQ | LE | LT | GE | GT | NE
  | ASSIGN
  | STRING of string
  | AND | BREAK | DO | ELSE | ELSEIF | END
  | FALSE | FOR | FUNCTION | IF | IN | LOCAL
  | NIL | NOT | OR | REPEAT | RETURN
  | THEN | TRUE | UNTIL | WHILE
  | IDENT of string
  | NUM of real

  val tokenToString : token -> string

  type 's state
  val mk : 's -> 's state
  val run : (char, 's) StringCvt.reader -> (token, 's state) StringCvt.reader
end
