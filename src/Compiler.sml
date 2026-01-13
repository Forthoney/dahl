structure Compiler =
struct
  exception Parse

  structure L = Lexer
  structure OP = Opcode

  structure Prec = Precedence
  structure CB = Chunk.Builder

  fun run rdr strm =
    let
      fun unary opcode cs = 
        let
          val (chunk, strm) = parsePrec Prec.unary cs
          val (operand, chunk) = CB.pop chunk
          val (dest, chunk) = CB.alloc chunk
        in
          (CB.emit (opcode (dest, operand), chunk), strm)
        end

      and binary opcode prec (chunk, strm) =
        let
          val l = CB.peek chunk
          val (chunk, strm) = parsePrec (prec + 1) (chunk, strm)
          val (r, chunk) = CB.pop chunk
          val dest = CB.peek chunk
        in
          (CB.emit (opcode (dest, l, r), chunk), strm)
        end

      and number n (chunk, strm) =
        let
          val (idx, chunk) = CB.addConst (Constant.NUM n, chunk)
          val (reg, chunk) = CB.alloc chunk
        in
          (CB.emit (OP.LOAD (reg, idx), chunk), strm)
        end

      and string s (chunk, strm) =
        let
          val (idx, chunk) = CB.addConst (Constant.STR s , chunk)
          val (reg, chunk) = CB.alloc chunk
        in
          (CB.emit (OP.LOAD (reg, idx), chunk), strm)
        end

      and literal opcode (chunk, strm) =
        let
          val (reg, chunk) = CB.alloc chunk
        in
          (CB.emit (opcode reg, chunk), strm)
        end

      and grouping cs =
        let val (chunk, strm) = expr cs
        in
          case rdr strm of
            SOME (L.R_PAREN, strm) => (chunk, strm)
          | _ => raise Parse
        end

      and parsePrec prec (chunk, strm) =
        case rdr strm of
          NONE => raise Fail "expect expression"
        | SOME (token, strm) =>
          let
            val prefixFn =
              case token of
                L.L_PAREN => grouping
              | L.SUB => unary OP.NEG
              | L.NOT => unary OP.NOT
              | L.NUM n => number n
              | L.STRING s => string s 
              | L.NIL => literal OP.LOAD_NIL
              | L.TRUE => literal OP.LOAD_TRUE
              | L.FALSE => literal OP.LOAD_FALSE
              | _ => raise Fail "expect expression" 
            val cs = prefixFn (chunk, strm)

            fun loop (chunk, strm) =
              case rdr strm of
                NONE => (chunk, strm)
              | SOME (token, strm') =>
                let val newPrec = Prec.ofToken token
                in
                  if prec > newPrec then (chunk, strm)
                  else
                    let
                      val infixFn =
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
                        | token => raise Fail ("unreachable..?" ^ Lexer.tokenToString token)
                    in
                      loop (infixFn newPrec (chunk, strm'))
                    end
                end
          in
            loop cs
          end

      and expr cs = parsePrec Prec.assign cs
      val (chunk, strm) = expr (CB.new, strm)
    in    
      CB.freeze chunk
    end
end
