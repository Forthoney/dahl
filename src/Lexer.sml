structure Lexer: LEXER =
struct
  structure SC = StringCvt
  val input1 = TextIO.StreamIO.input1

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
  | NUMBER of string
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
  fun isAssign ASSIGN = true
    | isAssign _ = false
  fun isComma COMMA = true
    | isComma _ = false
  fun isDot DOT = true
    | isDot _ = false
  fun isColon COLON = true
    | isColon _ = false
  fun isSemicolon SEMICOLON = true
    | isSemicolon _ = false
  fun isLParen LPAREN = true
    | isLParen _ = false
  fun isRParen RPAREN = true
    | isRParen _ = false

  val toString =
    fn AND => "AND"
     | BREAK => "BREAK"
     | DO => "DO"
     | ELSE => "ELSE"
     | ELSEIF => "ELSEIF"
     | END => "END"
     | FALSE => "FALSE"
     | FOR => "FOR"
     | FUNCTION => "FUNCTION"
     | IF => "IF"
     | IN => "IN"
     | LOCAL => "LOCAL"
     | NIL => "NIL"
     | NOT => "NOT"
     | OR => "OR"
     | REPEAT => "REPEAT"
     | RETURN => "RETURN"
     | THEN => "THEN"
     | TRUE => "TRUE"
     | UNTIL => "UNTIL"
     | WHILE => "WHILE"
     | NAME s => "NAME(" ^ s ^ ")"
     | NUMBER n => "NUMBER(" ^ n ^ ")"
     | STRING s => "STRING(" ^ String.toString s ^ ")"
     | PLUS => "PLUS"
     | MINUS => "MINUS"
     | STAR => "STAR"
     | SLASH => "SLASH"
     | MODULO => "MODULO"
     | CARET => "CARET"
     | EQEQ => "EQEQ"
     | NEQ => "NEQ"
     | LT => "LT"
     | GT => "GT"
     | LTE => "LTE"
     | GTE => "GTE"
     | CONCAT => "CONCAT"
     | LENGTH => "LENGTH"
     | LPAREN => "LPAREN"
     | RPAREN => "RPAREN"
     | LBRACK => "LBRACK"
     | RBRACK => "RBRACK"
     | LBRACE => "LBRACE"
     | RBRACE => "RBRACE"
     | ASSIGN => "ASSIGN"
     | COLON => "COLON"
     | SEMICOLON => "SEMICOLON"
     | COMMA => "COMMA"
     | DOT => "DOT"
     | VARARG => "VARARG"

  fun escape (c, strm) =
    let
      fun ctoi c = Char.ord c - Char.ord #"0"
      fun digit (3, acc) strm = (Char.chr acc, strm)
        | digit (count, acc) strm =
            case input1 strm of
              NONE => (Char.chr acc, strm)
            | SOME (c, strm') =>
                if Char.isDigit c then
                  digit (count + 1, acc * 10 + ctoi c) strm'
                else
                  (Char.chr acc, strm)
    in
      case c of
        #"a" => (#"\a", strm)
      | #"b" => (#"\b", strm)
      | #"f" => (#"\f", strm)
      | #"n" => (#"\n", strm)
      | #"r" => (#"\r", strm)
      | #"t" => (#"\t", strm)
      | #"v" => (#"\v", strm)
      | #"\\" => (#"\\", strm)
      | #"'" => (#"'", strm)
      | #"\"" => (#"\"", strm)
      | #"\n" => (#"\n", strm)
      | c => if Char.isDigit c then digit (1, ctoi c) strm else (c, strm)
    end

  fun scan stream =
    let
      fun name strm =
        let
          val (s, strm') =
            SC.splitl (fn c => c = #"_" orelse Char.isAlphaNum c) input1 strm
          val tok = 
            case s of
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
            | s => NAME s
        in
          (tok, strm')
        end

      fun dec strm =
        let
          val (s, strm) = SC.splitl Char.isDigit input1 strm
        in
          (NUMBER s, strm)
        end
        (* let *)
          (* fun loop acc strm = *)
            (* case input1 strm of *)
              (* NONE => (INT acc, strm) *)
            (* | SOME (c, strm') => *)
                (* if Char.isDigit c then *)
                  (* loop (acc * 10 + Char.ord c - Char.ord #"0") strm' *)
                (* else *)
                  (* (INT acc, strm) *)
        (* in *)
          (* loop 0 *)
        (* end *)

      fun hex prefix strm =
        let
          val (s, strm) = SC.splitl Char.isHexDigit input1 strm
          (* fun convert c = *)
            (* Word.andb (Word.fromInt (Char.ord c), 0wxF) *)
            (* + (if c > #"9" then 0wx9 else 0wx0) *)

          (* fun loop acc strm = *)
            (* case input1 strm of *)
              (* NONE => (INT (Word.toInt acc), strm) *)
            (* | SOME (c, strm') => *)
                (* if Char.isHexDigit c then loop (acc * 0wxF + convert c) strm' *)
                (* else (INT (Word.toInt acc), strm) *)
        in
          case s of
            "" => raise Fail "malformed hex number"
          | s => (NUMBER (prefix ^ s), strm)
        end

      fun shortString delim =
        let
          fun loop acc strm =
            case input1 strm of
              NONE => raise Fail "unterminated string"
            | SOME (#"\n", _) => raise Fail "unterminated string"
            | SOME (#"\\", strm') =>
                (case input1 strm' of
                   NONE => raise Fail "unfinished escape"
                 | SOME next =>
                     let val (c, strm'') = escape next
                     in loop (c :: acc) strm''
                     end)
            | SOME (c, strm') =>
                if c = delim then ((STRING o String.implode o rev) acc, strm')
                else loop (c :: acc) strm'
        in
          loop []
        end

      fun decode oldstrm (c, strm) =
        case c of
          #"{" => SOME (LBRACE, strm)
        | #"}" => SOME (RBRACE, strm)
        | #"[" => SOME (LBRACK, strm)
        | #"]" => SOME (RBRACK, strm)
        | #"(" => SOME (LPAREN, strm)
        | #")" => SOME (RPAREN, strm)
        | #"=" =>
            SOME
              (case input1 strm of
                 SOME (#"=", strm') => (EQEQ, strm')
               | _ => (ASSIGN, strm))
        | #";" => SOME (SEMICOLON, strm)
        | #":" => SOME (COLON, strm)
        | #"," => SOME (COMMA, strm)
        | #"." =>
            SOME
              (case input1 strm of
                 SOME (#".", strm') =>
                   (case input1 strm' of
                      SOME (#".", strm'') => (VARARG, strm'')
                    | _ => (CONCAT, strm'))
               | _ => (DOT, strm))
        | #"<" =>
            SOME
              (case input1 strm of
                 SOME (#"=", strm') => (LTE, strm')
               | _ => (LT, strm))
        | #">" =>
            SOME
              (case input1 strm of
                 SOME (#"=", strm') => (GTE, strm')
               | _ => (GT, strm))
        | #"~" =>
            SOME
              (case input1 strm of
                 SOME (#"=", strm') => (NEQ, strm')
               | _ => raise Fail "unexpected symbol after ~")
        | #"+" => SOME (PLUS, strm)
        | #"*" => SOME (STAR, strm)
        | #"/" => SOME (SLASH, strm)
        | #"%" => SOME (MODULO, strm)
        | #"^" => SOME (CARET, strm)
        | #"'" => SOME (shortString #"'" strm)
        | #"\"" => SOME (shortString #"\"" strm)
        | #"-" =>
            (case input1 strm of
               SOME (#"-", strm') =>
                 scan (SC.dropl (fn c => c = #"\n") input1 strm')
             | _ => SOME (MINUS, strm))
        | #"_" => SOME (name oldstrm)
        | #"0" =>
            SOME
              (case input1 strm of
                 SOME (#"x", strm') => hex "0x" strm'
               | SOME (#"X", strm') => hex "0X" strm'
               | _ => dec oldstrm)
        | c =>
            if Char.isDigit c then SOME (dec oldstrm)
            else if Char.isAlpha c then SOME (name oldstrm)
            else raise Fail "unknown char"

      val stream = SC.skipWS input1 stream
    in
      Option.composePartial (decode stream, input1) stream
    end
end
