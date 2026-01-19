structure Compiler =
struct
  exception Parse

  structure L = Lexer
  structure OP = Opcode

  structure Prec = Precedence
  structure CB = Chunk.Builder

  fun run rdr strm =
    let
      fun prefixFn canAssign token =
        case token of
          L.L_PAREN => grouping
        | L.SUB => unary OP.NEG
        | L.NOT => unary OP.NOT
        | L.NUM n => number n
        | L.STRING s => string s 
        | L.NIL => literal OP.LOAD_NIL
        | L.TRUE => literal OP.LOAD_TRUE
        | L.FALSE => literal OP.LOAD_FALSE
        | L.IDENT name => variable canAssign name
        | _ => raise Fail "expect expession" 

      and infixFn token =
        case token of
          L.ADD => binary OP.ADD
        | L.SUB => binary OP.SUB
        | L.MUL => binary OP.MUL
        | L.DIV => binary OP.DIV
        | L.MOD => binary OP.MOD
        | L.POW => binary OP.POW
        | L.LE => binary OP.LE
        | L.LT => binary OP.LT
        | L.GE => binary OP.GE
        | L.GT => binary OP.GT
        | L.EQ => binary OP.EQ
        | L.NE => binary OP.NE
        | _ => raise Fail ("unreachable..?" ^ Lexer.tokenToString token)
      
      and unary opcode st = 
        let
          val (scope, chunk, strm) = parsePrec Prec.unary st
          val (operand, chunk) = CB.pop chunk
          val (dest, chunk) = CB.alloc chunk
        in
          (scope, CB.emit (opcode (dest, operand), chunk), strm)
        end

      and binary opcode prec st =
        let
          val (scope, chunk, strm) = parsePrec (prec + 1) st
          val (r, chunk) = CB.pop chunk
          val (l, chunk) = CB.pop chunk
          val (dest, chunk) = CB.alloc chunk
        in
          (scope, CB.emit (opcode (dest, l, r), chunk), strm)
        end

      and number n (scope, chunk, strm) =
        let
          val (idx, chunk) = CB.addConst (Constant.NUM n, chunk)
          val (reg, chunk) = CB.alloc chunk
        in
          (scope, CB.emit (OP.LOAD (reg, idx), chunk), strm)
        end

      and string s (scope, chunk, strm) =
        let
          val (idx, chunk) = CB.addConst (Constant.STR s , chunk)
          val (reg, chunk) = CB.alloc chunk
        in
          (scope, CB.emit (OP.LOAD (reg, idx), chunk), strm)
        end

      and literal opcode (scope, chunk, strm) =
        let
          val (reg, chunk) = CB.alloc chunk
        in
          (scope, CB.emit (opcode reg, chunk), strm)
        end

      and grouping st =
        let val (scope, chunk, strm) = exp st
        in
          case rdr strm of
            SOME (L.R_PAREN, strm) => (scope, chunk, strm)
          | _ => raise Parse
        end

      and variable canAssign name st =
        namedVariable canAssign name st

      and namedVariable canAssign name (scope, chunk, strm) =
        case Scope.resolve scope name of
          SOME reg => (scope, CB.push (reg, chunk), strm)
        | NONE =>
          let
            val (id, chunk) = CB.addConst (Constant.STR name, chunk)
          in
            case (canAssign, rdr strm) of
              (true, SOME (L.ASSIGN, strm)) =>
              let val (scope, chunk, strm) = exp (scope, chunk, strm)
              in
                (scope, CB.emit (OP.SET_GLOBAL (CB.peek chunk, id), chunk), strm)
              end
            | _ =>
              let val (reg, chunk) = CB.alloc chunk
              in
                (scope, CB.emit (OP.GET_GLOBAL (reg, id), chunk), strm)
              end
          end

      and parsePrec prec (scope, chunk, strm) =
        case rdr strm of
          NONE => raise Fail "expect expession"
        | SOME (token, strm) =>
          let
            fun loop (scope, chunk, strm) =
              case rdr strm of
                NONE => (scope, chunk, strm)
              | SOME (token, strm') =>
                let val newPrec = Prec.ofToken token
                in
                  if prec > newPrec then (scope, chunk, strm)
                  else
                    loop (infixFn token newPrec (scope, chunk, strm'))
                end

            val canAssign = prec <= Prec.assign
            val st = prefixFn canAssign token (scope, chunk, strm)
            val (scope, chunk, strm) = loop st
          in
            case (canAssign, rdr strm) of
              (true, SOME (L.ASSIGN, _)) => raise Fail "invalid assignment target"
            | _ => (scope, chunk, strm)
          end

      and exp st = parsePrec Prec.assign st

      and localDecl [name] (scope, chunk, strm) =
        case rdr strm of
          SOME (L.ASSIGN, strm) =>
            let
              val (scope, chunk, strm) = exp (scope, chunk, strm)
            in
              (Scope.add scope name, chunk, strm)
            end
        | _ => raise Fail "expect assignment symbol"

      and stat (scope, chunk, strm) =
        case rdr strm of
          NONE => raise Fail "expect statement"
        | SOME (L.DO, strm) => 
          let
            fun loop (st as (scope, chunk, strm)) =
              case rdr strm of
                SOME (L.END, strm) => (Scope.exit scope, chunk, strm)
              | _ => loop (stat st)
          in
            loop (Scope.begin scope, chunk, strm)
          end
        | SOME (L.LOCAL, strm) =>
          (case rdr strm of
            SOME (L.IDENT name, strm) => localDecl [name] (scope, chunk, strm)
          | _ => raise Fail "expect name")
        | SOME _ => exp (scope, chunk, strm)
        | _ => raise Fail "unimplemented"

      and loop (scope, chunk, strm) =
        case rdr strm of
          NONE => CB.freeze chunk
        | SOME _ => loop (stat (scope, chunk, strm))
    in    
      loop (Scope.new, CB.new, strm)
    end
end
