(*
The MIT License

Copyright (c) 2020 Jason D. Nielsen <drjdnielsen@gmail.com> (original author)
Copyright (c) 2025 Seong-Heon jung <castlehoneyjung@gmail.com> (modified)
*)

{
open Token
}

let white = [' ' '\t']+
let newline = '\r'? '\n'
let decimal = ['0'-'9']+
let hex = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
let integer =  decimal | hex
let expo = ['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9']*
let float1 = int ('.' ['0'-'9']* )? expo?
let float2 = '.' ['0'-'9']+ expo?
let num = float1 | float2 | integer
let ident = [ 'a'-'z' 'A'-'Z' '_' ] [ 'a'-'z' 'A'-'Z' '0'-'9' '_' ]*
let single_str = '\'' ([^ '\'' '\\' ] | ([ '\\' ] [^ '_' ]))* '\''
let double_str = '\"' ([^ '\"' '\\' ] | ([ '\\' ] [^ '_' ]))* '\"'
let str = single_str | double_str
let long_str_start = '[' '='* '['
let long_comment = '-' '-' long_str
let long_end = ']' '='*

rule token = parse
  | white { token lexbuf }
  | newline { Lexing.new_line lexbuf }
  | "+"            { Plus }
  | "-"            { Minus }
  | "*"            { Mult }
  | "/"            { Div }
  | "%"            { Mod }
  | "^"            { Carat }
  | ">"            { GT }
  | "<"            { LT }
  | ">="           { GE }
  | "<="           { LE }
  | "=="           { EQ }
  | "~="           { NE }
  | "="            { Assign }
  | "."            { Dot }
  | ".."           { Cat }
  | "..."          { Ellipsis }
  | ":"            { Colon }
  | "::"           { Dcolon }  
  | ";"            { Semi }
  | ","            { Comma }
  | "#"            { Hash }
  | "{"            { LBrace }
  | "}"            { RBrace }
  | "("            { LParen }
  | ")"            { RParen }
  | "["            { LBracket }
  | "]"            { RBracket }  
  | "and"          { And }
  | "break"        { Break }
  | "do"           { Do }
  | "else"         { Else }
  | "elseif"       { Elseif }
  | "end"          { End }
  | "for"          { For }
  | "function"     { Function }
  | "goto"         { Goto }
  | "if"           { If }
  | "in"           { In }
  | "local"        { Local }
  | "not"          { Not }
  | "or"           { Or }
  | "repeat"       { Repeat }
  | "return"       { Return }
  | "then"         { Then }
  | "until"        { Until }
  | "while"        { While }
  | "<quit>"       { Eof }
  | "true"         { True }
  | "false"        { False }
  | "nil"          { Nil }
  | long_str_start { TODO }
  | long_comment   { TODO }
  | "--"           { comment lexbuf }
  | num            { Number (lexeme lexbuf) }
  | ident          { Ident (lexeme lexbuf) }
  | str            { Str (lexeme lexbuf) }
  | eof            { EOF }
  | 
