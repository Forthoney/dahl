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
        | _ => raise Fail "expect expression" 

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

      and exp st = parsePrec (Prec.assign + 1) st
      and assignment (scope, chunk, strm) =
        let
          fun parseAssignTarget strm =
            case rdr strm of
              SOME (L.IDENT name, strm) =>
              let
                fun loop (namesRev, strm) =
                  case rdr strm of
                    SOME (L.COMMA, strm) =>
                    (case rdr strm of
                      SOME (L.IDENT name, strm) => loop (name::namesRev, strm)
                    | _ => raise Fail "expect name")
                  | SOME (L.ASSIGN, strm) => SOME (rev namesRev, strm)
                  | _ => NONE
              in
                loop ([name], strm)
              end
            | _ => NONE

          fun parseExprList (scope, chunk, strm) =
            let
              val (scope, chunk, strm) = exp (scope, chunk, strm)
              val reg = CB.peek chunk
              fun loop (regsRev, scope, chunk, strm) =
                case rdr strm of
                  SOME (L.COMMA, strm) =>
                  let
                    val (scope, chunk, strm) = exp (scope, chunk, strm)
                    val reg = CB.peek chunk
                  in
                    loop (reg::regsRev, scope, chunk, strm)
                  end
                | _ => (rev regsRev, scope, chunk, strm)
            in
              loop ([reg], scope, chunk, strm)
            end

          fun assignTargets ([], _, chunk) = chunk
            | assignTargets (name::names, reg::regs, chunk) =
              let
                val (id, chunk) = CB.addConst (Constant.STR name, chunk)
                val chunk = CB.emit (OP.SET_GLOBAL (reg, id), chunk)
              in
                assignTargets (names, regs, chunk)
              end
            | assignTargets (name::names, [], chunk) =
              let
                val (nilReg, chunk) = CB.alloc chunk
                val chunk = CB.emit (OP.LOAD_NIL nilReg, chunk)
                val (id, chunk) = CB.addConst (Constant.STR name, chunk)
                val chunk = CB.emit (OP.SET_GLOBAL (nilReg, id), chunk)
              in
                assignTargets (names, [], chunk)
              end
        in
          case parseAssignTarget strm of
            SOME (names, strm) =>
            let
              val (regs, scope, chunk, strm) = parseExprList (scope, chunk, strm)
              val chunk = assignTargets (names, regs, chunk)
            in
              (scope, chunk, strm)
            end
          | NONE => parsePrec Prec.assign (scope, chunk, strm)
        end

      and localDecl [name] (scope, chunk, strm) =
        case rdr strm of
          SOME (L.ASSIGN, strm) =>
            let
              val (scope, chunk, strm) = exp (scope, chunk, strm)
            in
              (Scope.add scope name, chunk, strm)
            end
        | _ => raise Fail "expect assignment symbol"

      and ifStat st =
        let
          datatype term = T_END | T_ELSE | T_ELSEIF

          fun emitJump chunk =
            (CB.count chunk, CB.emit (OP.JMP 0, chunk))

          fun emitJumpIfFalse (reg, chunk) =
            (CB.count chunk, CB.emit (OP.JMP_IF_FALSE (reg, 0), chunk))

          fun patchJump (pos, target, chunk) =
            CB.patch (pos, OP.JMP (target - pos - 1), chunk)

          fun patchJumpIfFalse (pos, reg, target, chunk) =
            CB.patch (pos, OP.JMP_IF_FALSE (reg, target - pos - 1), chunk)

          fun patchAll endJumps chunk =
            let
              val target = CB.count chunk
              fun loop ([], chunk) = chunk
                | loop (pos::ps, chunk) = loop (ps, patchJump (pos, target, chunk))
            in
              loop (endJumps, chunk)
            end

          fun block (st as (currentScope, chunk, strm)) =
            case rdr strm of
              SOME (L.ELSEIF, strm) => (currentScope, chunk, strm, T_ELSEIF)
            | SOME (L.ELSE, strm) => (currentScope, chunk, strm, T_ELSE)
            | SOME (L.END, strm) => (currentScope, chunk, strm, T_END)
            | _ => block (stat st)

          fun elseBlock (st as (currentScope, chunk, strm)) =
            case rdr strm of
              SOME (L.END, strm) => (currentScope, chunk, strm)
            | _ => elseBlock (stat st)

          fun compileBranch (scope, chunk, strm, condReg, endJumps) =
            let
              val (falsePos, chunk) = emitJumpIfFalse (condReg, chunk)
              val (_, chunk, strm, term) = block (Scope.begin scope, chunk, strm)
              val (chunk, endJumps) =
                case term of
                  T_END => (chunk, endJumps)
                | _ =>
                  let val (jmpPos, chunk) = emitJump chunk
                  in (chunk, jmpPos::endJumps)
                  end
              val chunk = patchJumpIfFalse (falsePos, condReg, CB.count chunk, chunk)
            in
              case term of
                T_END => (scope, patchAll endJumps chunk, strm)
              | T_ELSE =>
                let
                  val (_, chunk, strm) = elseBlock (Scope.begin scope, chunk, strm)
                  val chunk = patchAll endJumps chunk
                in
                  (scope, chunk, strm)
                end
              | T_ELSEIF =>
                compileElseIf (scope, chunk, strm, endJumps)
            end

          and compileElseIf (scope, chunk, strm, endJumps) =
            let
              val (scope, chunk, strm) = exp (scope, chunk, strm)
              val condReg = CB.peek chunk
            in
              case rdr strm of
                SOME (L.THEN, strm) =>
                compileBranch (scope, chunk, strm, condReg, endJumps)
              | _ => raise Fail "expect 'then'"
            end

          val (scope, chunk, strm) = exp st
          val condReg = CB.peek chunk
        in
          case rdr strm of
            SOME (L.THEN, strm) => compileBranch (scope, chunk, strm, condReg, [])
          | _ => raise Fail "expect 'then'"
        end

      and whileStat (scope, chunk, strm) =
        let
          val loopStart = CB.count chunk
          val (scope, chunk, strm) = exp (scope, chunk, strm)
          val condReg = CB.peek chunk

          fun emitJumpIfFalse (reg, chunk) =
            (CB.count chunk, CB.emit (OP.JMP_IF_FALSE (reg, 0), chunk))

          fun patchJumpIfFalse (pos, reg, target, chunk) =
            CB.patch (pos, OP.JMP_IF_FALSE (reg, target - pos - 1), chunk)

          fun block (st as (_, chunk, strm)) =
            case rdr strm of
              SOME (L.END, strm) => (scope, chunk, strm)
            | _ => block (stat st)
        in
          case rdr strm of
            SOME (L.DO, strm) =>
            let
              val (exitPos, chunk) = emitJumpIfFalse (condReg, chunk)
              val (_, chunk, strm) = block (Scope.begin scope, chunk, strm)
              val backPos = CB.count chunk
              val backOffset = loopStart - backPos - 1
              val chunk = CB.emit (OP.JMP backOffset, chunk)
              val chunk = patchJumpIfFalse (exitPos, condReg, CB.count chunk, chunk)
            in
              (scope, chunk, strm)
            end
          | _ => raise Fail "expect 'do'"
        end

      and stat (scope, chunk, strm) =
        case rdr strm of
          NONE => raise Fail "expect statement"
        | SOME (L.DO, strm) => 
          let
            fun block (st as (_, chunk, strm)) =
              case rdr strm of
                SOME (L.END, strm) => (scope, chunk, strm)
              | _ => block (stat st)
          in
            block (Scope.begin scope, chunk, strm)
          end
        | SOME (L.LOCAL, strm) =>
          (case rdr strm of
            SOME (L.IDENT name, strm) => localDecl [name] (scope, chunk, strm)
          | _ => raise Fail "expect name")
        | SOME (L.IF, strm) => ifStat (scope, chunk, strm)
        | SOME (L.WHILE, strm) => whileStat (scope, chunk, strm)
        | SOME _ => assignment (scope, chunk, strm)

      and loop (scope, chunk, strm) =
        case rdr strm of
          NONE => CB.freeze chunk
        | SOME _ => loop (stat (scope, chunk, strm))
    in    
      loop (Scope.new, CB.new, strm)
    end
end
