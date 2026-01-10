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

  structure Builder =
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
      (OP.K size, {code, reg, const = {content = V.Number n :: content, size = size + 1}})

    fun alloc {code, const, reg} =
      (OP.R reg, {code, const, reg = reg + 1})

    val new = {code = [], const = {content = [], size = 0}, reg = 0}

    fun pop {code, const, reg} =
      (OP.R (reg - 1), {code, const, reg = reg - 1})

    fun peek {code, const, reg} = OP.R (reg - 1)
  end

  fun compile () =
    let
      val st = Builder.new

      val (k, st) = Builder.addNum (1.5, st)
      val (l, st) = Builder.alloc st
      val st = Builder.emit ((OP.LOAD (l, k), ~1), st)

      val (k, st) = Builder.addNum (2.5, st)
      val (r, st) = Builder.alloc st
      val st = Builder.emit ((OP.LOAD (r, k), ~1), st)

      val st = Builder.emit ((OP.ADD (r, l, r), ~1), st)

      val st = Builder.emit ((OP.RET (OP.R 0, OP.R 2), ~1), st)
    in
      Builder.freeze st
    end
end
