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

  fun tokenToString tok =
    case tok of
      COMMENT s => "COMMENT: " ^ String.toString s
    | ERR s => "ERR: " ^ s
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
              val inner = acc ^ inner
              val lineno = lineno + 0
            in
              case rdr src of
                SOME (#"]", src) =>
                let val (eqs, src) = SC.splitl (fn c => c = #"=") rdr src
                in
                  if String.size eqs <> level then loop (inner ^ "]" ^ eqs) src
                  else
                    case rdr src of
                      SOME (#"]", src) => (TERM inner, (src, lineno))
                    | _ => (UNTERM inner, (src, lineno))
                end
              | _ => (UNTERM inner, (src, lineno))
            end
        in
          loop ""
        end
        
      fun comment src =
        let
          val (comment, src) =
            SC.splitl (fn c => c <> #"\n" andalso c <> #"\r") rdr src
        in
          case rdr src of
            SOME (#"\r", src) =>
            (case rdr src of
              SOME (#"\n", src) => (COMMENT (comment ^ "\r\n"), (src, lineno + 1))
            | _ => (COMMENT (comment ^ "\r"), (src, lineno + 1)))
          | SOME (#"\n", src) => (COMMENT (comment ^ "\n"), (src, lineno + 1))
          | NONE => (COMMENT comment, (src, lineno))
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
              | _ => SOME (comment src)
            end
          | _ => SOME (comment src))
        | _ => emit (SUB, src)
    in
      case rdr src of
        NONE => NONE
      | SOME (#"\r", src) =>
        (case rdr src of
          SOME (#"\n", src) => run rdr (src, lineno + 1)
        | _ => run rdr (src, lineno + 1))
      | SOME (#" " | #"\t" | #"\v" | #"\f", src) => run rdr (src, lineno)
      | SOME (#"\n", src) => run rdr (src, lineno + 1)
      | SOME (#"+", src) => emit (ADD, src)
      | SOME (#"-", src) => hyphen src
      | SOME (#"*", src) => emit (MUL, src)
      | SOME (#"/", src) => emit (DIV, src)
      | SOME (#"%", src) => emit (MOD, src)
      | SOME (#"^", src) => emit (POW, src)
      | SOME (#"#", src) => emit (LEN, src)
      | SOME (#"(", src) => emit (L_PAREN, src)
      | SOME (#")", src) => emit (R_PAREN, src)
      | SOME (#"[", src) => emit (L_BRACK, src)
      | SOME (#"]", src) => emit (R_BRACK, src)
      | SOME (#"{", src) => emit (L_BRACE, src)
      | SOME (#"}", src) => emit (R_BRACE, src)
      | SOME (#":", src) => emit (COLON, src)
      | SOME (#";", src) => emit (SEMICOLON, src)
      | SOME (#",", src) => emit (COMMA, src)
      | SOME (#".", src) =>
        emit (case rdr src of
          SOME (#".", src) =>
          (case rdr src of
            SOME (#".", src) => (VARARG, src)
          | _ => (CONCAT, src))
        | _ => (DOT, src))
      | SOME (#"=", src) =>
        emit (case rdr src of
          SOME (#"=", src) => (EQ, src)
        | _ => (ASSIGN, src))
      | SOME (#"<", src) =>
        emit (case rdr src of
          SOME (#"=", src) => (LE, src)
        | _ => (LT, src))
      | SOME (#">", src) =>
        emit (case rdr src of
          SOME (#"=", src) => (GE, src)
        | _ => (GT, src))
      | SOME (#"~", src) =>
        emit (case rdr src of
          SOME (#"=", src) => (NE, src)
        | _ => (ERR "~", src))
    end
end
