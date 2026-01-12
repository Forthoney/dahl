structure Machine =
struct
  val numRegs = 250

  structure V = Value
  structure AS = ArraySlice
  structure OP = Opcode

  exception Type
  
  fun interpret chunk =
    let
      val regs = Array.array (numRegs, V.Nil)
      fun set (dest, v) = Array.update (regs, dest, v)
      fun get r = Array.sub (regs, r)

      and loop (chunk as {code, const, line}, ip) =
        let
          fun numeric opr (OP.R dest, OP.R l, OP.R r) =
            case (get l, get r) of
              (V.Number l, V.Number r) =>
              (set (dest, opr (l, r)); loop (chunk, ip + 1))
            | _ => raise Type

          fun negate (OP.R dest, OP.R src) =
            case get src of
              V.Number n =>
              (set (dest, V.Number (~n)); loop (chunk, ip + 1))
            | _ => raise Type

          fun not_ (OP.R dest, OP.R src) =
            let
              val v = case get src of
                V.Boolean false | V.Nil => true
              | _ => false
            in
              (set (dest, V.Boolean v); loop (chunk, ip + 1))
            end
        in
          case Vector.sub (code, ip) of
            OP.LOAD (OP.R dest, OP.K k) =>
            (set (dest, Vector.sub (const, k)); loop (chunk, ip + 1))
          | OP.LOAD_NIL (OP.R dest) =>
            (set (dest, Value.Nil); loop (chunk, ip + 1))
          | OP.LOAD_TRUE (OP.R dest) =>
            (set (dest, Value.Boolean true); loop (chunk, ip + 1))
          | OP.LOAD_FALSE (OP.R dest) =>
            (set (dest, Value.Boolean false); loop (chunk, ip + 1))
          | OP.NEG opr => negate opr
          | OP.NOT opr => not_ opr
          | OP.ADD opr => numeric (V.Number o op+) opr
          | OP.SUB opr => numeric (V.Number o op-) opr
          | OP.MUL opr => numeric (V.Number o op*) opr
          | OP.DIV opr => numeric (V.Number o op/) opr
          | OP.MOD opr => numeric (V.Number o Real.rem) opr
          | OP.POW opr => numeric (V.Number o Math.pow) opr
          | OP.LE opr => numeric (V.Boolean o op<=) opr
          | OP.LT opr => numeric (V.Boolean o op<) opr
          | OP.GE opr => numeric (V.Boolean o op>=) opr
          | OP.GT opr => numeric (V.Boolean o op>) opr
          | OP.EQ (OP.R dest, OP.R l, OP.R r) =>
            (set (dest, (V.Boolean o V.eq) (get l, get r)); loop (chunk, ip + 1))
          | OP.NE (OP.R dest, OP.R l, OP.R r) =>
            (set (dest, (V.Boolean o not o V.eq) (get l, get r)); loop (chunk, ip + 1))
          | OP.RET (OP.R from, OP.R to) => AS.slice (regs, from, SOME to)
        end
    in
      loop (chunk, 0)
    end
end
