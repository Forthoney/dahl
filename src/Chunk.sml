structure Chunk : CHUNK =
struct
  structure OP = Opcode
  structure V = Value
  type t =
    { code : OP.t vector
    , const : Value.t vector
    , line : int vector
    }

  fun disassemble {code, const, line} =
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

  structure Builder =
  struct
    type obj =
      { code : (OP.t * int) list
      , const : {content : Value.t list, size : int}
      , reg : int
      }
    val new = {code = [], const = {content = [], size = 0}, reg = 0}

    fun freeze ({code, const = {content, size}, reg} : obj) =
      { code = (Vector.fromList o map #1 o rev) code
      , const = Vector.fromList (rev content)
      , line = (Vector.fromList o map #2 o rev) code
      }

    fun emit (opcode, {code, const, reg}) =
      { code = (opcode, ~1)::code
      , const = const
      , reg = reg
      }

    fun addNum (n, {code, const = {content, size}, reg}) =
      (OP.K size, {code, reg, const = {content = V.Number n :: content, size = size + 1}})

    fun alloc {code, const, reg} =
      (OP.R reg, {code, const, reg = reg + 1})

    fun pop {code, const, reg} =
      (OP.R (reg - 1), {code, const, reg = reg - 1})

    fun peek {code, const, reg} = OP.R (reg - 1)
  end
end
