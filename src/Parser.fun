functor ParserFn
  (val tokenScan: (Lexer.token, TextIO.StreamIO.instream) StringCvt.reader):
  PARSER =
struct
  open Lexer
  open AST

  fun consume pred strm =
    case tokenScan strm of
      SOME (t, strm) => if pred t then SOME strm else NONE
    | NONE => NONE

  fun tryConsume pred strm =
    case tokenScan strm of
      SOME (t, strm') => if pred t then strm' else strm
    | NONE => strm

  fun name strm =
    case tokenScan strm of
      SOME (NAME s, strm) => SOME (s, strm)
    | _ => NONE

  (* prefixexp ::= Name
                 | prefixexp '[' exp ']'
                 | prefixexp '.' Name
                 | prefixexp args
                 | prefixexp ':' Name args
                 | '(' exp ')'             *)
  fun prefixExp strm =
    let
      (* prefixexp '[' exp ']' *)
      fun index prev strm =
        case exp strm of 
          NONE => raise Fail "invalid index"
        | SOME (exp, strm) =>
          case tokenScan strm of
            SOME (RBRACK, strm) => loop (PVar (VIndex (prev, exp))) strm
          | _ => raise Fail "missing ']'"

      (* prefixexp '.' Name *)
      and field prev strm =
        case tokenScan strm of
          SOME (NAME n, strm) => loop (PVar (VField (prev, name))) strm
        | _ => raise Fail "expected name of field"

      (* prefixexp ':' Name args *)
      and method prev strm =
        case tokenScan strm of
          SOME (NAME n, strm) =>
          (case tokenScan strm of
            SOME (LPAREN, strm) => loop (PFnCall (Method (prev, args))) strm
          | _ => raise Fail "missing ')'")
        | _ => raise Fail "missing method name"

      and loop prev strm =
        case tokenScan strm of
          SOME (DOT, strm) => field prev strm
        | SOME (LBRACK, strm) => index prev strm
        | SOME (COLON, strm) => method prev strm
        | _ => prev

      val (start, strm) = 
        case tokenScan strm of
          SOME (NAME n, strm) => (PVar (VName n), strm)
        | SOME (LPAREN, strm) =>
          (case exp strm of
            NONE => raise Fail "invalid prefix expression"
          | SOME (exp, strm) =>
            case consume isRParen of
              NONE => raise Fail "invalid prefix expression"
            | SOME strm => (PExp exp, strm))
        | _ => raise Fail "invalid prefix expression"
    in
      loop start strm
    end

  and args strm =
    case tokenScan strm of
      SOME (STRING s, strm) => (AString s, strm)
    | SOME (LPAREN, strm) =>
      let
        val (args, strm) = Reader.repeat exp {sep = SOME comma} strm
      in
        case tokenScan strm of
          SOME (RPAREN, strm) => (AExp args, strm)
        | _ => raise Fail "missing )"
      end
    | _ => raise Fail "unimplemented"

  and exp strm =
    case tokenScan strm of
      SOME (NIL, strm) => SOME (ENil, strm)
    | SOME (TRUE, strm) => SOME (ETrue, strm)
    | SOME (FALSE, strm) => SOME (ETrue, strm)
    | _ => NONE

  fun localAssign strm =
    let
      val (names, strm) = Reader.repeat name {sep = SOME (consume isComma)} strm
    in
      case tokenScan strm of
        SOME (ASSIGN, strm) =>
          (case Reader.repeat exp {sep = SOME (consume isComma)} strm of
             ([], _) => raise Fail "no expression following local assign"
           | (explist, strm) => (LocalAssign (names, explist), strm))
      | _ => (LocalAssign (names, []), strm)
    end

  fun funcName strm =
    case Reader.repeat name {sep = SOME (consume isDot)} strm of
      ([], _) => raise Fail "no name given for function declaration"
    | (names, strm) =>
        case consume isColon strm of
          NONE => {names = names, method = NONE}
        | SOME strm =>
            case name strm of
              SOME (method, strm) => {names = names, method = SOME method}
            | NONE => raise Fail "no method name given for function declaration"

  fun funcBody strm =
    let
      val ({params, vararg}, strm) =
        case consume (fn LPAREN => true | _ => false) strm of
          NONE => raise Fail "function body does not have list of params"
        | SOME strm =>
            let
              val (params, strm) = Reader.repeat name {sep = SOME (consume isComma)} strm
              val termFail = Fail
                "function parameter list not properly terminated"
              fun rParenCheck strm =
                case tokenScan strm of
                  SOME (RPAREN, strm) =>
                    ({params = params, vararg = true}, strm)
                | _ => raise termFail
            in
              case (params, tokenScan strm) of
                (_, SOME (RPAREN, strm)) =>
                  ({params = params, vararg = false}, strm)
              | ([], SOME (VARARG, strm)) => rParenCheck strm
              | ([], _) => raise termFail
              | (params, SOME (COMMA, strm)) =>
                  (case tokenScan strm of
                     SOME (VARARG, strm) => rParenCheck strm
                   | _ => raise termFail)
              | _ => raise termFail
            end
      val (blk, strm) = Option.valOf (block strm)
    in
      case consume (fn END => true | _ => false) strm of
        SOME strm => ({params = params, vararg = vararg, block = blk}, strm)
      | NONE => raise Fail "function body does not end with \"end\""
    end

  and localFunction strm =
    case name strm of
      NONE => raise Fail "no name for local function"
    | SOME (name, strm) =>
        let val (body, strm) = funcBody strm
        in (LocalFunction (name, body), strm)
        end

  and fnCall strm =
    let
      fun fnArgs strm =
        case args strm of
          NONE => raise Fail "no argument provided"
        | SOME (args, strm) => SOME (Function (pexp, args), strm)

      fun method strm =
        case name strm of
          NONE => raise Fail "no method name provided"
        | SOME (name, strm) => 
          case args strm of
            NONE => raise Fail "no argument provided"
          | SOME (args, strm) => SOME (Method (pexp, name, args), strm)
    in
      case prefixExp strm of
        NONE => NONE
      | SOME (pexp, strm) =>
        case consume isColon strm of
          NONE => fnArgs strm
        | SOME strm => method strm
    end

  and stat strm =
    case tokenScan strm of
      NONE => NONE
    | SOME (NAME s, _) =>
      let
        val (names, strm) = Reader.repeat name {sep = SOME (consume isComma)} strm
      in
        case consume isAssign strm of
          NONE => raise Fail "variable declaration must be followed by \"=\""
        | SOME strm =>
          case Reader.repeat name {sep = SOME (consume isComma)} strm of
            ([], _) => raise Fail "variable declaration must be followed by expressions"
          | (exps, strm) => 
      end
    | SOME (LOCAL, strm) =>
        (case tokenScan strm of
           SOME (NAME s, _) => SOME (localAssign strm)
         | SOME (FUNCTION, strm) => SOME (localFunction strm)
         | _ => raise Fail "invalid local declaration")
    | _ => NONE

  and laststat strm =
    case tokenScan strm of
      SOME (BREAK, strm) => SOME (Break, strm)
    | SOME (RETURN, strm) =>
      let
        val (exps, strm) = Reader.repeat exp {sep = SOME (consume isComma)} strm
      in
        SOME (Return exps, tryConsume isSemicolon strm)
      end
    | _ => NONE

  and block strm =
    let
      fun block' _ =
        let
          val (stats, strm) =
            Reader.repeat stat {sep = SOME (SOME o tryConsume isSemicolon)} strm
        in
          case laststat strm of
            SOME (ls, strm) => (Block (stats, SOME ls), strm)
          | NONE => (Block (stats, NONE), strm)
        end
    in
      Option.compose (block', tokenScan) strm
    end

  val scan = block
end
