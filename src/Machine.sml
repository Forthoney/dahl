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

      fun run (chunk as {code, const, line}, ip) =
        let
          fun binOp opr (OP.R dest, OP.R l, OP.R r) =
            case (get l, get r) of
              (V.Number l, V.Number r) =>
              (set (dest, V.Number (opr (l, r))); run (chunk, ip + 1))
            | _ => raise Type
        in
          case Vector.sub (code, ip) of
            OP.LOAD (OP.R dest, OP.K k) =>
            (set (dest, Vector.sub (const, k)); run (chunk, ip + 1))
          | OP.NEG (OP.R dest, OP.R src) =>
            (set (dest, get src); run (chunk, ip + 1))
          | OP.ADD opr => binOp op+ opr
          | OP.SUB opr => binOp op- opr
          | OP.MUL opr => binOp op* opr
          | OP.DIV opr => binOp op/ opr
          | OP.MOD opr => binOp Real.rem opr
          | OP.POW opr => binOp Math.pow opr
          | OP.RET (OP.R from, OP.R to) => AS.slice (regs, from, SOME to)
        end
    in
      run (chunk, 0)
    end
end
