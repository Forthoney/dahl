signature LEXER =
sig
  datatype token =
    COMMENT of string
  | ERR of string
  | ADD | SUB | MUL | DIV | MOD | POW
  | LEN
  | L_PAREN | R_PAREN | L_BRACK | R_BRACK | L_BRACE | R_BRACE
  | COLON | SEMICOLON | COMMA
  | DOT | CONCAT | VARARG
  | EQ | LE | LT | GE | GT | NE
  | ASSIGN

  val tokenToString : token -> string

  type 's state
  val mk : 's -> 's state
  val run : (char, 's) StringCvt.reader -> (token, 's state) StringCvt.reader
end
