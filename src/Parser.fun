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

  (* === Expression Parsing ===
     The strategy mimics not the EBNF in the reference manual,
     but instead the actual implementation in lparser.c *)

  fun terminalExp strm =
    case tokenScan strm of
      SOME (LPAREN, strm) =>
      let val (exp, strm) = expr strm
      in
        (PExp exp, strm)
      end
    | SOME (NAME n, strm) => (PVar (VName n), strm)
    | _ => raise Fail "expected terminal prefix expression"

  and prefixExp strm =
    let
      fun loop (prev: prefix_exp, strm) =
        case tokenScan strm of
          SOME (DOT, strm) => field (prev, strm)
        | SOME (LBRACK, strm) => index (prev, strm)
        | SOME (COLON, strm) => method (prev, strm)
        | SOME (LPAREN | STRING _ | LBRACE, strm) =>
          let
            val (args, strm) = funcArgs strm
          in
            loop (PFnCall (Function (prev, args)), strm)
          end
        | _ => (EPrefix prev, strm)

      and field (prev, strm) =
        case tokenScan strm of
          SOME (NAME n, strm) => loop (PVar (VField (prev, n)), strm)
        | _ => raise Fail "expected name of field"

      and index (prev, strm) =
        let
          val (exp, strm) = expr strm
        in
          case tokenScan strm of
            SOME (RBRACK, strm) => loop (PVar (VIndex (prev, exp)), strm)
          | _ => raise Fail "expected ]"
        end

      and method (prev, strm) =
        case tokenScan strm of
          SOME (NAME n, strm) =>
          let
            val (args, strm) = funcArgs strm
          in
            loop (PFnCall (Method (prev, n, args)), strm)
          end
        | _ => raise Fail "missing method name"
    in
      loop (terminalExp strm)
    end

  and funcArgs strm =
    case tokenScan strm of
      SOME (STRING s, strm) => (AString s, strm)
    | SOME (LPAREN, strm) =>
      (case tokenScan strm of
        SOME (RPAREN, strm) => (AExp [], strm)
      | _ =>
        let val (args, strm) = exprList strm
        in
          case tokenScan strm of
            SOME (RPAREN, strm) => (AExp args, strm)
          | _ => raise Fail "expected )"
        end)
    | SOME (LBRACE, strm) => raise Fail "unimplemented"
    | _ => raise Fail "invalid function argument"

  and subexpr limit strm =
    let
      fun unary operator strm =
        let
          val (expr, strm) = subexpr 8 strm
        in
          (EUnary (operator, expr), strm)
        end

      fun loop (lhs, strm) =
        let
          fun binary operator (lPrio, rPrio) strm' =
            if lPrio <= limit then (lhs, strm)
            else
              let
                val (rhs, strm) = subexpr rPrio strm'
              in
                (EBinary (lhs, operator, rhs), strm)
              end
        in
          case tokenScan strm of
            SOME (PLUS, strm') => binary Plus (6, 6) strm'
          | SOME (MINUS, strm') => binary Minus (6, 6) strm'
          | SOME (STAR, strm') => binary Mult (7, 7) strm'
          | SOME (SLASH, strm') => binary Div (7, 7) strm'
          | SOME (MODULO, strm') => binary Mod (7, 7) strm'
          | SOME (CARET, strm') => binary Pow (10, 9) strm'
          | SOME (CONCAT, strm') => binary Concat (5, 4) strm'
          | SOME (EQEQ, strm') => binary Eq (3, 3) strm'
          | SOME (NEQ, strm') => binary Neq (3, 3) strm'
          | SOME (LT, strm') => binary Lt (3, 3) strm'
          | SOME (GT, strm') => binary Gt (3, 3) strm'
          | SOME (LTE, strm') => binary Lte (3, 3) strm'
          | SOME (GTE, strm') => binary Gte (3, 3) strm'
          | SOME (AND, strm') => binary And (2, 2) strm'
          | SOME (OR, strm') => binary Or (1, 1) strm'
          | _ => (lhs, strm)
        end

      val lhs = 
        case tokenScan strm of
          SOME (NOT, strm) => unary Not strm
        | SOME (MINUS, strm) => unary Neg strm
        | SOME (LENGTH, strm) => unary Length strm
        | SOME (NUMBER n, strm) => (ENumber n, strm)
        | SOME (STRING s, strm) => (EString s, strm)
        | SOME (NIL, strm) => (ENil, strm)
        | SOME (TRUE, strm) => (ETrue, strm)
        | SOME (FALSE, strm) => (EFalse, strm)
        | SOME (VARARG, strm) => (EVararg, strm)
        | _ => prefixExp strm
    in
      loop lhs
    end

  and expr strm = subexpr 0 strm

  and exprList strm =
    let
      fun loop acc strm = 
        case tokenScan strm of
          SOME (COMMA, strm) =>
          let
            val (arg, strm) = expr strm
          in
            loop (arg::acc) strm
          end
        | _ => (rev acc, strm)

      val (arg0, strm) = expr strm
    in
      loop [arg0] strm
    end

  fun localAssign strm =
    let
      val (names, strm) = Reader.repeat name {between = SOME (consume isComma)} strm
    in
      case tokenScan strm of
        SOME (ASSIGN, strm) =>
        let
          val (exprs, strm) = exprList strm
        in
          (LocalAssign (names, exprs), strm)
        end
      | _ => (LocalAssign (names, []), strm)
    end

  fun funcName strm =
    case Reader.repeat name {between = SOME (consume isDot)} strm of
      ([], _) => raise Fail "no name given for function declaration"
    | (names, strm) =>
        case tokenScan strm of
          SOME (COLON, strm) =>
          (case name strm of
            SOME (method, strm) => {names = names, method = SOME method}
          | NONE => raise Fail "no method name given for function declaration")
        | _ => {names = names, method = NONE}

  fun funcBody strm =
    let
      val ({params, vararg}, strm) =
        case tokenScan strm of
          SOME (LPAREN, strm) => 
            (let
              val (params, strm) = Reader.repeat name {between = SOME (consume isComma)} strm
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
            end)
        | _ => raise Fail "function body does not have list of params"
      val (blk, strm) = Option.valOf (block strm)
    in
      case tokenScan strm of
        SOME (END, strm) => ({params = params, vararg = vararg, block = blk}, strm)
      | _ => raise Fail "function body does not end with \"end\""
    end

  and localFunction strm =
    case name strm of
      NONE => raise Fail "no name for local function"
    | SOME (name, strm) =>
        let val (body, strm) = funcBody strm
        in (LocalFunction (name, body), strm)
        end

  and stat strm =
    case tokenScan strm of
      NONE => NONE
    (* | SOME (NAME s, _) => *)
      (* let *)
        (* val (names, strm) = Reader.repeat name {sep = SOME (consume isComma)} strm *)
      (* in *)
        (* case consume isAssign strm of *)
          (* NONE => raise Fail "variable declaration must be followed by \"=\"" *)
        (* | SOME strm => *)
          (* case Reader.repeat name {sep = SOME (consume isComma)} strm of *)
            (* ([], _) => raise Fail "variable declaration must be followed by expressions" *)
          (* | (exps, strm) => SOME (Assign (names, exps), strm) *)
      (* end *)
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
      (* (case tokenScan strm of *)
        (* SOME (SEMICOLON, strm) => SOME (Return [], strm) *)
      (* | NONE => SOME (Return [], strm) *)
      (* | SOME _ => *)
        (* let *)
          (* fun loop acc strm = *)
            (* let *)
              (* val (exp, strm) = expr strm *)
            (* in *)
              (* case tokenScan strm of *)
                (* SOME (SEMICOLON, strm) => SOME (Return (rev acc), strm) *)
              (* | SOME (COMMA, strm) => loop (exp :: acc) strm *)
              (* | _ => SOME (Return (rev acc), strm) *)
            (* end *)
        (* in *)
          (* loop [] strm *)
        (* end) *)
      SOME (Return [], strm)
    | _ => NONE

  and block strm =
    let
      fun block' _ =
        let
          fun terminatedStat strm =
            case stat strm of
              SOME (s, strm) => SOME (s, tryConsume isSemicolon strm)
            | NONE => NONE
          val (stats, strm) = Reader.repeat terminatedStat {between = NONE} strm
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
