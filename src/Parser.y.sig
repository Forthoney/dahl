signature Lua_TOKENS =
sig
  type ('a, 'b) token
  type svalue
  val UMINUS: 'a * 'a -> (svalue, 'a) token
  val NUMBER: (real) * 'a * 'a -> (svalue, 'a) token
  val STRING: (string) * 'a * 'a -> (svalue, 'a) token
  val NAME: (string) * 'a * 'a -> (svalue, 'a) token
  val VARARG: 'a * 'a -> (svalue, 'a) token
  val CONCAT: 'a * 'a -> (svalue, 'a) token
  val DOT: 'a * 'a -> (svalue, 'a) token
  val COMMA: 'a * 'a -> (svalue, 'a) token
  val COLON: 'a * 'a -> (svalue, 'a) token
  val SEMI: 'a * 'a -> (svalue, 'a) token
  val RBRACK: 'a * 'a -> (svalue, 'a) token
  val LBRACK: 'a * 'a -> (svalue, 'a) token
  val RBRACE: 'a * 'a -> (svalue, 'a) token
  val LBRACE: 'a * 'a -> (svalue, 'a) token
  val RPAREN: 'a * 'a -> (svalue, 'a) token
  val LPAREN: 'a * 'a -> (svalue, 'a) token
  val ASSIGN: 'a * 'a -> (svalue, 'a) token
  val GE: 'a * 'a -> (svalue, 'a) token
  val GT: 'a * 'a -> (svalue, 'a) token
  val LE: 'a * 'a -> (svalue, 'a) token
  val LT: 'a * 'a -> (svalue, 'a) token
  val NEQ: 'a * 'a -> (svalue, 'a) token
  val EQ: 'a * 'a -> (svalue, 'a) token
  val LEN: 'a * 'a -> (svalue, 'a) token
  val POW: 'a * 'a -> (svalue, 'a) token
  val MOD: 'a * 'a -> (svalue, 'a) token
  val DIV: 'a * 'a -> (svalue, 'a) token
  val TIMES: 'a * 'a -> (svalue, 'a) token
  val MINUS: 'a * 'a -> (svalue, 'a) token
  val PLUS: 'a * 'a -> (svalue, 'a) token
  val WHILE: 'a * 'a -> (svalue, 'a) token
  val UNTIL: 'a * 'a -> (svalue, 'a) token
  val TRUE: 'a * 'a -> (svalue, 'a) token
  val THEN: 'a * 'a -> (svalue, 'a) token
  val RETURN: 'a * 'a -> (svalue, 'a) token
  val REPEAT: 'a * 'a -> (svalue, 'a) token
  val OR: 'a * 'a -> (svalue, 'a) token
  val NOT: 'a * 'a -> (svalue, 'a) token
  val NIL: 'a * 'a -> (svalue, 'a) token
  val LOCAL: 'a * 'a -> (svalue, 'a) token
  val IN: 'a * 'a -> (svalue, 'a) token
  val IF: 'a * 'a -> (svalue, 'a) token
  val FUNCTION: 'a * 'a -> (svalue, 'a) token
  val FOR: 'a * 'a -> (svalue, 'a) token
  val FALSE: 'a * 'a -> (svalue, 'a) token
  val END: 'a * 'a -> (svalue, 'a) token
  val ELSEIF: 'a * 'a -> (svalue, 'a) token
  val ELSE: 'a * 'a -> (svalue, 'a) token
  val DO: 'a * 'a -> (svalue, 'a) token
  val BREAK: 'a * 'a -> (svalue, 'a) token
  val AND: 'a * 'a -> (svalue, 'a) token
  val EOF: 'a * 'a -> (svalue, 'a) token
end
signature Lua_LRVALS =
sig
  structure Tokens: Lua_TOKENS
  structure ParserData: PARSER_DATA
  sharing type ParserData.Token.token = Tokens.token
  sharing type ParserData.svalue = Tokens.svalue
end
