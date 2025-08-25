{
open Token

let keyword = function
  | "and" -> AND | "break" -> BREAK | "do" -> DO
  | "else" -> ELSE | "elseif" -> ELSEIF | "end" -> END
  | "false" -> FALSE | "for" -> FOR | "function" -> FUNCTION
  | "if" -> IF | "in" -> IN | "local" -> LOCAL
  | "nil" -> NIL | "not" -> NOT | "or" -> OR
  | "repeat" -> REPEAT | "return" -> RETURN
  | "then" -> THEN | "true" -> TRUE
  | "until" -> UNTIL | "while" -> WHILE
  | _ as id -> IDENT id


open Lexing

let buf = Buffer.create 256
let push_char = Buffer.add_char buf
let push_string = Buffer.add_string buf
let clear_buf () = Buffer.reset buf

let unexpected c lexbuf =
  let p = lexeme_start_p lexbuf in
  failwith (Printf.sprintf "Unexpected character %C at line %d, column %d" c p.pos_lnum (p.pos_cnum - p.pos_bol))

let escape = function
  | 'a'->'\007'
  | 'b'->'\008'
  | 'f'->'\012'
  | 'n'->'\n'
  | 'r'->'\r'
  | 't'->'\t'
  | 'v'->'\011'
  | '\\'->'\\'
  | '\''->'\''
  | '\"'->'\"'
  | _ -> failwith "unreachable"

let numeric_escape s =
  match int_of_string s with
  | n when 0 <= n && n < 256 -> Char.chr n
  | _ -> invalid_arg "decimal escape out of range"

let count_level = String.fold_left (fun cnt c -> if c = '=' then cnt + 1 else cnt) 0
}


let newline = ['\n' '\r'] | "\r\n" | "\n\r"
let space = [' ' '\t' '\012' '\013']
let digit = ['0'-'9']
let alpha = ['A'-'Z''a'-'z''_']
let alnum = alpha | digit

let dec_int = digit+
let dec_frac = digit* '.' digit+ | digit+ '.' digit*
let dec_exp = ['e' 'E'] ['+' '-']? digit+
let dec_float = dec_frac (dec_exp)? | dec_int dec_exp

let ident = alpha alnum*
let lb_open = '[' '='* '['
let lb_close = ']' '='* ']'

rule token = parse
  | newline { token lexbuf }
  | space+  { token lexbuf }
  | "--" lb_open as s { long_comment (count_level s) lexbuf; token lexbuf }
  | "--" [^ '\n' '\r']* { token lexbuf }
  | (lb_open as s) {
    long_string (count_level s) lexbuf;
    STRING (Buffer.contents buf)
  }
  | ('\''|'"' as delim) {
    clear_buf ();
    short_string delim lexbuf;
    STRING (Buffer.contents buf)
  }
  | (dec_float as s) { NUMBER s }
  | (dec_int as s) { NUMBER s }
  | (ident as s) { keyword s }
  | "..."   { ELLIPSIS }
  | ".."    { CONCAT }
  | "."     { DOT }
  | "=="    { EQ }
  | "~="  { NEQ }
  | "<="  { LTE }
  | ">="  { GTE }
  | "<"   { LT }
  | ">"   { GT }
  | "="   { ASSIGN }
  | "+"   { PLUS }
  | "-"   { MINUS }
  | "*"   { STAR }
  | "/"   { SLASH }
  | "%"   { MODULO }
  | "^"   { CARET }
  | "#"   { HASH }
  | "("   { LPAREN }
  | ")"   { RPAREN }
  | "{"   { LBRACE }
  | "}"   { RBRACE }
  | "["   { LBRACKET }
  | "]"   { RBRACKET }
  | ";"   { SEMICOLON }
  | ":"   { COLON }
  | ","   { COMMA }
  | eof   { EOF }
  | _ as c { unexpected c lexbuf }
  and short_string delim = parse
    | '\n' | '\r' { failwith "Unterminated short string literal" }
    | '\\' ('a'|'b'|'f'|'n'|'r'|'t'|'v'|'\\'|'\''|'\"' as c) { push_char (escape c); short_string delim lexbuf }
    | '\\' (digit digit? digit? as s) { push_char (numeric_escape s); short_string delim lexbuf }
    | '\\' newline { push_char '\n'; short_string delim lexbuf }
    | (_ as c) { if c = delim then () else (push_char c; short_string delim lexbuf) }
    | eof { failwith "Unterminated short string literal" }
  and long_string eqs = parse
    | ']' '='* ']' as s {
        let level = count_level s in
        if level = eqs then ()
        else (push_string s; long_string eqs lexbuf)
      }
    | _ as c { push_char c; long_string eqs lexbuf }
    | eof { failwith "Unterminated long string" }
  and long_comment eqs = parse
    | ']' '='* ']' as s {
        let level = count_level s in
        if level = eqs then ()
        else (long_comment eqs lexbuf)
      }
    | _ { long_comment eqs lexbuf }
    | eof { () }
