structure Chunk =
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

  structure State =
  struct
    type t =
      { code : (OP.t * int) list
      , const : {content : Value.t list, size : int}
      , reg : int
      }

    fun freeze ({code, const = {content, size}, reg} : t) =
      { code = (Vector.fromList o map #1 o rev) code
      , const = Vector.fromList (rev content)
      , line = (Vector.fromList o map #2 o rev) code
      }

    fun emit (cl, {code, const, reg}) =
      { code = cl::code
      , const = const
      , reg = reg
      }

    fun addNum (n, {code, const = {content, size}, reg}) =
      (OP.K size, {code = code, const = {content = V.Number n :: content, size = size +1}, reg = reg})

    fun alloc {code, const, reg} =
      (OP.R reg, {code = code, const = const, reg = reg + 1})

    val new = {code = [], const = {content = [], size = 0}, reg = 0}
  end

  fun compile () =
    let
      val st = State.new

      val (k, st) = State.addNum (1.5, st)
      val (l, st) = State.alloc st
      val st = State.emit ((OP.LOAD (l, k), ~1), st)

      val (k, st) = State.addNum (2.5, st)
      val (r, st) = State.alloc st
      val st = State.emit ((OP.LOAD (r, k), ~1), st)

      val st = State.emit ((OP.ADD (r, l, r), ~1), st)

      val st = State.emit ((OP.RET (OP.R 0, OP.R 2), ~1), st)
    in
      State.freeze st
    end
end
