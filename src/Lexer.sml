structure Lexer :> LEXER =
struct
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

  fun tokenToString tok =
    case tok of
      COMMENT s => "COMMENT: " ^ String.toString s
    | STRING s => "STRING: " ^ String.toString s
    | IDENT name => "IDENT: " ^ name
    | NUM n => "NUM: " ^ Real.toString n
    | ADD => "'+'"
    | SUB => "'-'"
    | MUL => "'*'"
    | DIV => "'/'"
    | MOD => "'%'"
    | POW => "'^'"
    | LEN => "'#'"
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

  fun countLines s = 
    let
      fun f (c, (count, prev)) =
        case (c, prev) of
          (#"\r", _) => (count + 1, c)
        | (#"\n", #"\r") => (count, c)
        | (#"\n", _) => (count + 1, c)
        | (_, _) => (count, c)
      val (finalCount, _) = Vector.foldl f (0, #" ") s
    in
      finalCount
    end

  type 's state = 's * int
  fun mk s = (s, 0)
  fun run rdr (src, lineno) =
    let
      fun emit (tok, src) = SOME (tok, (src, lineno))

      fun longBracket cons level src =
        let
          val acc = ref ""
          fun loop src =
            let
              val (inner, src) = SC.splitl (fn c => c <> #"]") rdr src
              val _ = acc := !acc ^ inner
              val SOME (#"]", src) = rdr src
              val (eqs, src) = SC.splitl (fn c => c = #"=") rdr src
            in
              if String.size eqs <> level then
                (acc := !acc ^ "]"; loop src)
              else
                let val SOME (#"]", src) = rdr src
                in SOME (cons (!acc), (src, lineno + countLines (!acc)))
                end
            end
        in
          loop src
          handle Bind => raise Unterminated {inner = !acc, expected = "]]", lineno = lineno + countLines (!acc)}
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
                SOME (#"[", src) => longBracket COMMENT level src
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

      fun literalString double src =
        let
          val pat = if double then "\\\r\n\"" else "\\\r\n'"
          val delim = if double then "\"" else "'"
          fun loop acc (src, lineno) =
            let
              val (inner, src) = SC.splitl (not o Char.contains pat) rdr src
              val acc = acc ^ inner
            in
              case rdr src of
                NONE | SOME (#"\r" | #"\n", _) =>
                raise Unterminated {inner = acc, expected = delim, lineno = lineno}
              | SOME (#"\"" | #"'", src) => (STRING acc, (src, lineno))
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
                | NONE =>
                  raise Unterminated {inner = acc, expected = delim, lineno = lineno})
              | SOME (c, src) => raise Fail "unreachable"
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
        | (#"\"", src) => literalString true src
        | (#"'", src) => literalString false src
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
          | _ => raise Invalid {found = "~", lineno = lineno})
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
            raise Fail ("invalid..?" ^ String.str c)
      , rdr) src 
    end
end
