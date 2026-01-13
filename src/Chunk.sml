structure Chunk : CHUNK =
struct
  structure OP = Opcode
  structure V = Value

  structure CT = ConstTable
  structure CB = ConstTable.Builder

  type t =
    { code : OP.t vector
    , consts : CT.t
    , line : int vector
    }

  fun disassemble {code, consts, line} =
    let
      fun fmt (0, x, acc) = Int.toString (Vector.sub (line, 0)) ^ "\t" ^ OP.disassemble x
        | fmt (i, x, acc) =
          let
            val lino = Vector.sub (line, i)
            val prevLino = Vector.sub (line, i - 1)
            val lineTxt = if lino = prevLino then "|" else Int.toString lino
          in
            acc ^ "\n" ^ lineTxt ^ "\t" ^ OP.disassemble x
          end
    in
      Vector.foldli fmt "" code
    end

  fun getConst ({code, consts, line}, id) =
    CT.get consts id

  structure Builder =
  struct
    type obj =
      { code : (OP.t * int) list
      , consts : CB.obj
      , reg : int
      }
    val new = {code = [], consts = CB.new (), reg = 0}

    fun freeze ({code, consts, reg} : obj) =
      { code = (Vector.fromList o map #1 o rev) code
      , consts = CB.freeze consts
      , line = (Vector.fromList o map #2 o rev) code
      }

    fun emit (opcode, {code, consts, reg}) =
      { code = (opcode, ~1)::code
      , consts = consts
      , reg = reg
      }

    fun addConst (k, {code, consts, reg}) =
      let val id = CB.get consts k
      in (id, {code, reg, consts})
      end

    fun alloc {code, consts, reg} =
      (OP.R reg, {code, consts, reg = reg + 1})

    fun pop {code, consts, reg} =
      (OP.R (reg - 1), {code, consts, reg = reg - 1})

    fun peek {code, consts, reg} = OP.R (reg - 1)
  end
end
