structure Compiler =
struct
  exception Parse

  structure L = Lexer
  structure OP = Opcode

  structure Precedence =
  struct
    val assign = 1
    val or = 2
    val and' = 3
    val cmp = 4
    val concat = 5
    val term = 6
    val factor = 7
    val unary = 8
    val pow = 9
    val call = 10
    val primary = 11

    fun ofToken token =
      case token of
        L.SUB | L.ADD => term
      | L.MUL | L.DIV | L.MOD => factor
      | _ => 0
  end

  structure Prec = Precedence
  structure CB = Chunk.Builder

  fun run rawStrm =
    let
      val rdr = Lexer.run (Stream.Char.flatten TextIO.StreamIO.inputLine)

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
          val (chunk, strm) = parsePrec (Prec.ofToken L.ADD) (chunk, strm)
          val (r, chunk) = CB.pop chunk
          val (dest, chunk) = CB.pop chunk
        in
          (CB.emit (OP.ADD (dest, l, r), chunk), strm)
        end

      and number n (chunk, strm) =
        let
          val (idx, chunk) = CB.addNum (n, chunk)
          val (reg, chunk) = CB.alloc chunk
        in
          (CB.emit (OP.LOAD (reg, idx), chunk), strm)
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
              | L.NUM n => number n
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
                        | _ => raise Fail "unreachable..?"
                    in
                      infixFn newPrec (chunk, strm')
                    end
                end
          in
            loop cs
          end

      and expr cs = parsePrec Prec.assign cs

      val strm = (Lexer.mk o Stream.Char.mk o TextIO.getInstream) rawStrm
    in    
      expr (CB.new, strm)
    end
end
