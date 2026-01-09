structure Compiler =
struct
  structure State :>
  sig
    type t
    val empty : t
    val toString : t -> string
    val alloc : t -> Bytecode.reg * t
    val emit : Bytecode.t * t -> t
    val addString : string * t -> Bytecode.const * t
    val addNum : real * t -> Bytecode.const * t
    val tryFoldBoolLit : t -> t option
  end =
  struct
    datatype entry = String of string | Number of real
    fun entryEq (String s1, String s2) = s1 = s2
      | entryEq (Number r1, Number r2) = Real.== (r1, r2)
      | entryEq _ = false

    type t =
      { consts: {content : entry list, size : int}
      , freeReg : int (* next free register *)
      , ins : Bytecode.t list
      }

    val empty = {consts = {content = [], size = 0}, freeReg = 0, ins = []}

    fun toString {consts = {content, size}, freeReg, ins} =
      let
        fun entryToString (String s) = "\"" ^ s ^ "\""
          | entryToString (Number n) = Real.toString n
        val consts =
          (Array.foldli (fn (i, x, acc) => acc ^ Int.toString i ^ ":" ^ entryToString x ^ "\n") ""
          o Array.fromList o rev) content
      in
        consts ^ (String.concatWith "\n" o rev o map Bytecode.toString) ins ^ "\n"
      end

    fun alloc {consts, freeReg, ins} = (Bytecode.R freeReg, {consts, freeReg = freeReg + 1, ins})
    fun emit (newIns, {consts, freeReg, ins}) = {consts, freeReg, ins = newIns :: ins}

    fun findOrInsert (tgt, consts as {content, size}) =
      let
        fun loop _ [] = (size, {content = tgt::content, size = size + 1})
          | loop acc (x::xs) =
            if entryEq (x, tgt) then
              (size - acc - 1, {content, size})
            else
              loop (acc + 1) xs
      in
        loop 0 content
      end

    fun addString (s, {consts, freeReg, ins}) =
      let val (idx, consts) = findOrInsert (String s, consts)
      in (Bytecode.K idx, {consts, freeReg, ins})
      end
    fun addNum (n, {consts, freeReg, ins}) =
      let val (idx, consts) = findOrInsert (Number n, consts)
      in (Bytecode.K idx, {consts, freeReg, ins})
      end

    fun tryFoldBoolLit {consts, freeReg, ins} =
      case ins of
        Bytecode.LoadBool (reg, b) :: ins => SOME {consts, freeReg, ins = Bytecode.LoadBool (reg, not b) :: ins} 
      | _ => NONE
  end

  structure A = AST
  structure B = Bytecode
  structure S = State

  fun compileExp (exp, state) =
    case exp of
      A.Nil =>
      let val (newReg, state) = S.alloc state
      in (newReg, S.emit (B.LoadNil newReg, state))
      end
    | A.Bool b =>
      let val (newReg, state) = S.alloc state
      in (newReg, S.emit (B.LoadBool (newReg, b), state))
      end
    | A.Number n =>
      let
        val (newConst, state) = S.addNum (n, state)
        val (newReg, state) = S.alloc state
      in
        (newReg, S.emit (B.Load (newReg, newConst), state))
      end
    | A.String s =>
      let
        val (newConst, state) = S.addString (s, state)
        val (newReg, state) = S.alloc state
      in
        (newReg, S.emit (B.Load (newReg, newConst), state))
      end
    | A.UnOp (A.Not, e) =>
      let
        val (e, state) = compileExp (e, state)
      in
        case S.tryFoldBoolLit state of
          SOME state => (e, state)
        | NONE =>
          let val (newReg, state) = S.alloc state
          in (newReg, S.emit (B.Not (newReg, e), state))
          end
      end
    | A.UnOp (opr, e) =>
      let
        val (e, state) = compileExp (e, state)
        val (newReg, state) = S.alloc state
      in
        (newReg, S.emit ((B.fromUnaryOp opr) (newReg, e), state))
      end
    | A.BinOp (opr, l, r) =>
      let
        val (l, state) = compileExp (l, state)
        val (r, state) = compileExp (r, state)
        val (newReg, state) = S.alloc state
      in
        (newReg, S.emit ((B.fromBinaryOp opr) (newReg, B.R_ l, B.R_ r), state))
      end
    | A.TableCons fs =>
      let
        val (tReg, state) = S.alloc state
        val (arr, tbl) =
          List.foldl (fn (field, (arr, tbl)) =>
            case field of
              A.FieldKey _ | A.FieldProp _ => (arr, tbl + 1)
            | A.FieldSeq _ => (arr + 1, tbl)) (0, 0) fs
        val state = S.emit (B.NewTable (tReg, arr, tbl), state)

        fun loop (field, (cnt, state)) =
          case field of
            A.FieldKey (k, v) =>
            let
              val (k, state) = compileExp (k, state)
              val (v, state) = compileExp (v, state)
            in
              (cnt, S.emit (B.SetTable (tReg, B.R_ k, B.R_ v), state))
            end
          | A.FieldProp (prop, v) =>
            let
              val (k, state) = S.addString (prop, state)
              val (v, state) = compileExp (v, state)
            in
              (cnt, S.emit (B.SetTable (tReg, B.K_ k, B.R_ v), state))
            end
          | A.FieldSeq v =>
            let
              val cnt = cnt + 1
              val (k, state) = S.addNum (Real.fromInt cnt, state)
              val (v, state) = compileExp (v, state)
            in
              (cnt, S.emit (B.SetTable (tReg, B.K_ k, B.R_ v), state))
            end

        val (_, state) = List.foldl loop (0, state) fs
      in
        (tReg, state)
      end
      
  fun compile (A.Block [A.Return [e]]) =
    compileExp (e, S.empty)
end
