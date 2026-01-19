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

  fun dumpConsts ({consts, ...} : t) =
    let
      val vec = CT.toVector consts
      fun fmt (i, c, acc) =
        let
          val line = Int.toString i ^ ":\t" ^ V.toString (Constant.toValue c)
        in
          case acc of
            "" => line
          | _ => acc ^ "\n" ^ line
        end
    in
      Vector.foldli fmt "" vec
    end

  fun getConst ({code, consts, line}, id) =
    CT.get consts id

  structure Builder =
  struct
    type obj =
      { code : (OP.t * int) list
      , consts : CB.obj
      , reg : int
      , aliased : int list
      }
    val new = {code = [], consts = CB.new (), reg = 0, aliased = []}

    fun freeze ({code, consts, ...} : obj) =
      { code = (Vector.fromList o map #1 o rev) code
      , consts = CB.freeze consts
      , line = (Vector.fromList o map #2 o rev) code
      }

    fun emit (opcode, line, {code, consts, reg, aliased}) =
      { code = (opcode, line)::code
      , consts
      , reg
      , aliased
      }

    fun count ({code, ...} : obj) = length code

    fun patch (idx, opcode, {code, consts, reg, aliased}) =
      let
        val len = length code
        val revIdx = len - 1 - idx

        fun update (i, []) = raise Fail "patch out of bounds"
          | update (0, (_, line)::rest) = (opcode, line)::rest
          | update (i, x::xs) = x::update (i - 1, xs)
      in
        if revIdx < 0 then raise Fail "patch out of bounds"
        else
          { code = update (revIdx, code)
          , consts
          , reg
          , aliased
          }
      end

    fun addConst (k, {code, consts, reg, aliased}) =
      let val id = CB.get consts k
      in (id, {code, reg, consts, aliased})
      end

    fun alloc {code, consts, reg, aliased} =
      (OP.R reg, {code, consts, reg = reg + 1, aliased})

    fun pop {code, consts, reg, aliased = []} =
        (OP.R (reg - 1), {code, consts, reg = reg - 1, aliased = []})
      | pop {code, consts, reg, aliased = x::xs} =
        (OP.R x, {code, consts, reg, aliased = xs})

    fun peek {code, consts, reg, aliased} = OP.R (reg - 1)

    fun push (r, {code, consts, reg, aliased}) =
      {code, consts, reg, aliased = r::aliased}
  end
end
