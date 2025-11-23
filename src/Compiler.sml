structure Compiler =
struct
  open AST
  open Bytecode
  structure V = Value

  fun newReg state =
    (0, state)

  fun addConst (state, v) =
    (0, state)

  fun emit (state, ins) =
    state

  fun exp (state, e) =
    let
      fun literal v =
        let
          val (src, state) = addConst (state, v)
          val (dest, state) = newReg state
          val state = emit (state, Load (dest, src))
        in
          (dest, state)
        end

      fun table fields = 
        let
          val (tbl, state) = newReg state
          val state = emit (state, NewTable tbl)
          fun field (f, {state, seq}) =
            let
              val (seq, k, v) =
                case f of
                  FieldKey (k, v) => (seq, k, v)
                | FieldProp (prop, v) => (seq, String prop, v)
                | FieldSeq v => (seq + 1, Int seq, v)
              val (k, state) = exp (state, k)
              val (v, state) = exp (state, v)
            in
              {state = emit (state, SetTable ({table = tbl, key = k, val = v})), seq = seq}
            end
          val init = {state = state, seq = 0, acc = []}
          val {state, ...} = List.foldl field init fields 
        in
          (tbl, state)
        end
    in
      case e of
        Nil => literal V.Nil
      | Bool b => literal (V.Bool b)
      | Number n => literal (V.Number n)
      | String s => literal (V.String s)
      | BinOp (opr, l, r) =>
        let
          val (l, state) = exp (state, l)
          val (r, state) = exp (state, r)
          val (dest, state) = newReg state
          val state = emit (state, Add (l, r))
        in
          (dest, state)
        end
      | UnOp (opr, v) =>
        let
          val (v, state) = exp (state, v)
          val (dest, state) = newReg state
          val state = emit (state, Not v)
        in 
          (dest, state)
        end
      | TableCons fields => table fields
      | Func ({params, vararg}, block) => raise Fail "unimplemented"
    end
end
