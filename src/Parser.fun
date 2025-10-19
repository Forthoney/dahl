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

  fun exp strm =
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

  (* prefixexp is exceptional somewhat amongst the parsing rules
     because it is right-recursive.
     Hence, it requires an inner loop. *)
  and prefixExp strm =
    let
      (* fun loop acc strm = *)
        (* case tokenScan strm of *)
          (* NONE => () *)
        (* | SOME (NAME s, strm) => loop (VName s :: acc) strm *)
        (* | SOME () *)
    in
      case tokenScan strm of
        NONE => NONE
      | SOME (NAME s, strm) => SOME (PVar s, strm)
      | SOME () => ()
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
