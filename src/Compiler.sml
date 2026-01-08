structure Compiler =
struct
  structure A = AST
  structure BC = Bytecode

  datatype entry = String of string | Number of real
  fun entryToString (String s) = "\"" ^ s ^ "\""
    | entryToString (Number n) = Real.toString n

  fun compileExp (exp, acc as (consts as {content, size}, nRegs, ins)) =
    case exp of
      A.Nil => (consts, nRegs + 1, BC.LoadNil nRegs :: ins)
    | A.Bool b => (consts, nRegs + 1, BC.LoadBool (nRegs, b) :: ins)
    | A.Number n =>
      ({content = Number n :: content, size = size + 1}, nRegs + 1, BC.Load (nRegs, size) :: ins)
    | A.String s =>
      ({content = String s :: content, size = size + 1}, nRegs + 1, BC.Load (nRegs, size) :: ins)
    | A.UnOp (opr, e) =>
      let val (consts, nRegs, ins) = compileExp (e, acc)
      in
        (consts, nRegs + 1, (BC.fromUnaryOp opr) (nRegs, nRegs - 1) :: ins)
      end
    | A.BinOp (opr, l, r) =>
      let
        val acc as (_, nRegs, _) = compileExp (l, acc)
        val (consts, nRegs', ins) = compileExp (r, acc)
      in
        (consts, nRegs' + 1, (BC.fromBinaryOp opr) (nRegs', nRegs - 1, nRegs' - 1) :: ins)
      end

  fun compile (A.Block [A.Return [e]]) =
    compileExp (e, ({content = [], size = 0}, 0, []))

  fun toDebug ({content, size}, _, ins) =
    let
      val constTbl =
        (Array.foldli (fn (i, x, acc) => acc ^ Int.toString i ^ ":" ^ entryToString x ^ "\n") "" o Array.fromList o rev) content
    in
      constTbl ^ (String.concatWith "\n" o rev o map BC.toString) ins ^ "\n"
    end
end
