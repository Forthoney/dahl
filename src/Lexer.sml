structure Lexer :> LEXER =
struct
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
  | STRING of string
  | AND | BREAK | DO | ELSE | ELSEIF | END
  | FALSE | FOR | FUNCTION | IF | IN | LOCAL
  | NIL | NOT | OR | REPEAT | RETURN
  | THEN | TRUE | UNTIL | WHILE
  | IDENT of string
  | NUM of real

  fun tokenToString tok =
    case tok of
      COMMENT s => "COMMENT: " ^ String.toString s
    | STRING s => "STRING: " ^ String.toString s
    | ERR s => "ERR: " ^ s
    | IDENT name => "IDENT: " ^ name
    | NUM n => "NUM: " ^ Real.toString n
    | ADD => "'+'"
    | SUB => "'-'"
    | MUL => "'*'"
    | DIV => "'/'"
    | MOD => "'%'"
    | POW => "'^'"
    | L_PAREN => "'('"
    | R_PAREN => "')'"
    | L_BRACK => "'['"
    | R_BRACK => "']'"
    | L_BRACE => "'{'"
    | R_BRACE => "'}'"
    | COLON => "':'"
    | SEMICOLON => "';'"
    | COMMA => "','"
    | DOT => "'.'"
    | CONCAT => "'..'"
    | VARARG => "'...'"
    | EQ => "'=='"
    | LE => "'<='"
    | LT => "'<'"
    | GE => "'>='"
    | GT => "'>'"
    | NE => "'~='"
    | ASSIGN => "'='"
    | AND => "'and'"
    | BREAK => "'break'"
    | DO => "'do'"
    | ELSE => "'else'"
    | ELSEIF => "'elseif'"
    | END => "'end'"
    | FALSE => "'false'"
    | FOR => "'for'"
    | FUNCTION => "'function'"
    | IF => "'if'"
    | IN => "'in'"
    | LOCAL => "'local'"
    | NIL => "'nil'"
    | NOT => "'not'"
    | OR => "'or'"
    | REPEAT => "'repeat'"
    | RETURN => "'return'"
    | THEN => "'then'"
    | TRUE => "'true'"
    | UNTIL => "'until'"
    | WHILE => "'while'"

  structure SC = StringCvt

  type 's state = 's * int
  fun mk s = (s, 0)
  fun run rdr (src, lineno) =
    let
      fun emit (tok, src) = SOME (tok, (src, lineno))

      datatype long_bracket = TERM of string | UNTERM of string
      fun longBracket level =
        let
          fun loop acc src =
            let
              val (inner, src) = SC.splitl (fn c => c <> #"]") rdr src
              val acc = acc ^ inner
              val lineno = lineno + 0 (* this is wrong *)
            in
              case rdr src of
                SOME (#"]", src) =>
                let val (eqs, src) = SC.splitl (fn c => c = #"=") rdr src
                in
                  if String.size eqs <> level then loop (acc ^ "]" ^ eqs) src
                  else
                    case rdr src of
                      SOME (#"]", src) => (TERM acc, (src, lineno))
                    | _ => (UNTERM acc, (src, lineno))
                end
              | _ => (UNTERM acc, (src, lineno))
            end
        in
          loop ""
        end
        
      fun lineComment src =
        let
          val (cmt, src) =
            SC.splitl (fn c => c <> #"\n" andalso c <> #"\r") rdr src
          val token = 
            case rdr src of
              SOME (#"\r", src) =>
              (case rdr src of
                SOME (#"\n", src) => (COMMENT (cmt ^ "\r\n"), (src, lineno + 1))
              | _ => (COMMENT (cmt ^ "\r"), (src, lineno + 1)))
            | SOME (#"\n", src) => (COMMENT (cmt ^ "\n"), (src, lineno + 1))
            | NONE => (COMMENT cmt, (src, lineno))
        in
          SOME token
        end

      fun hyphen src =
        case rdr src of
          SOME (#"-", src) =>
          (case rdr src of
            SOME (#"[", src') =>
            let
              val (eqs, src'') = SC.splitl (fn c => c = #"=") rdr src'
              val level = String.size eqs
            in
              case rdr src'' of
                SOME (#"[", src) =>
                (case longBracket level src of
                  (TERM cmt, next) => SOME (COMMENT cmt, next)
                | (UNTERM cmt, next) => SOME (ERR cmt, next))
              | _ => lineComment src
            end
          | _ => lineComment src)
        | _ => emit (SUB, src)

      fun dots src =
        emit (case rdr src of
          SOME (#".", src) =>
          (case rdr src of
            SOME (#".", src) => (VARARG, src)
          | _ => (CONCAT, src))
        | _ => (DOT, src))

      fun literalString src =
        let
          fun loop acc (src, lineno) =
            let
              val (inner, src) = SC.splitl (not o Char.contains "\\\r\n\"") rdr src
              val acc = acc ^ inner
            in
              case rdr src of
                NONE | SOME (#"\r" | #"\n", _) => (ERR acc, (src, lineno))
              | SOME (#"\"", src) => (STRING acc, (src, lineno))
              | SOME (#"\\", src) =>
                (case rdr src of
                  SOME (#"a", src) => loop (acc ^ "\a") (src, lineno)
                | SOME (#"b", src) => loop (acc ^ "\b") (src, lineno)
                | SOME (#"n", src) => loop (acc ^ "\n") (src, lineno)
                | SOME (#"r", src) => loop (acc ^ "\r") (src, lineno)
                | SOME (#"t", src) => loop (acc ^ "\t") (src, lineno)
                | SOME (#"v", src) => loop (acc ^ "\v") (src, lineno)
                | SOME (#"\\", src) => loop (acc ^ "\\") (src, lineno)
                | SOME (#"\"", src) => loop (acc ^ "\"") (src, lineno)
                | SOME (#"'", src) => loop (acc ^ "'") (src, lineno)
                | SOME (#"\n", src) => loop (acc ^ "\n") (src, lineno + 1)
                | SOME (c, src) => loop (acc ^ String.str c) (src, lineno)
                | NONE => (ERR acc, (src, lineno)))
              | SOME (c, src) => raise Fail (String.str c)
            end
        in
          SOME (loop "" (src, lineno))
        end
    in
      Option.composePartial
      (fn (#" " | #"\t" | #"\v" | #"\f", src) => run rdr (src, lineno)
        | (#"\n", src) => run rdr (src, lineno + 1)
        | (#"+", src) => emit (ADD, src)
        | (#"-", src) => hyphen src
        | (#"*", src) => emit (MUL, src)
        | (#"/", src) => emit (DIV, src)
        | (#"%", src) => emit (MOD, src)
        | (#"^", src) => emit (POW, src)
        | (#"#", src) => emit (LEN, src)
        | (#"(", src) => emit (L_PAREN, src)
        | (#")", src) => emit (R_PAREN, src)
        | (#"[", src) => emit (L_BRACK, src)
        | (#"]", src) => emit (R_BRACK, src)
        | (#"{", src) => emit (L_BRACE, src)
        | (#"}", src) => emit (R_BRACE, src)
        | (#":", src) => emit (COLON, src)
        | (#";", src) => emit (SEMICOLON, src)
        | (#",", src) => emit (COMMA, src)
        | (#".", src) => dots src
        | (#"\"", src) => literalString src
        | (#"\r", src) =>
          run rdr (case rdr src of
            SOME (#"\n", src) => (src, lineno + 1)
          | _ => (src, lineno + 1))
        | (#"=", src) =>
          emit (case rdr src of
            SOME (#"=", src) => (EQ, src)
          | _ => (ASSIGN, src))
        | (#"<", src) =>
          emit (case rdr src of
            SOME (#"=", src) => (LE, src)
          | _ => (LT, src))
        | (#">", src) =>
          emit (case rdr src of
            SOME (#"=", src) => (GE, src)
          | _ => (GT, src))
        | (#"~", src) =>
          emit (case rdr src of
            SOME (#"=", src) => (NE, src)
          | _ => (ERR "~", src))
        | (c, _) =>
          if Char.isAlpha c orelse c = #"_" then
            let
              fun pred c = Char.isAlpha c orelse c = #"_" orelse Char.isDigit c 
              val (word, src) = SC.splitl pred rdr src
              val token = 
                case word of
                  "and" => AND 
                | "break" => BREAK 
                | "do" => DO 
                | "else" => ELSE 
                | "elseif" => ELSEIF 
                | "end" => END 
                | "false" => FALSE 
                | "for" => FOR 
                | "function" => FUNCTION 
                | "if" => IF 
                | "in" => IN 
                | "local" => LOCAL 
                | "nil" => NIL 
                | "not" => NOT 
                | "or" => OR 
                | "repeat" => REPEAT 
                | "return" => RETURN 
                | "then" => THEN 
                | "true" => TRUE 
                | "until" => UNTIL 
                | "while" => WHILE 
                | _ => IDENT word 
            in
              emit (token, src)
            end
          else if Char.isDigit c then
            case Real.scan rdr src of
              SOME (r, src) => emit (NUM r, src)
            | NONE => raise Fail "unreachable"
          else
            raise Fail "invalid..?"
      , rdr) src 
    end
end
