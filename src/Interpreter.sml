structure Interpreter =
struct
  open Bytecode 
  structure V = Value

  val vars = Array.array (750, V.Nil)

  fun sparseDump () =
    ( print "Printing non-Nil values\n"
    ; Array.appi
        (fn (_, V.Nil) => ()
          | (i, V.Boolean b) => print (Int.toString i ^ ":" ^ Bool.toString b ^ "\n")
          | (i, V.Number n) => print (Int.toString i ^ ":" ^ Real.toString n ^ "\n"))
        vars
    )

  fun setReg (R idx) v = Array.update (vars, idx, v)
  fun getReg (R idx) = Array.sub (vars, idx)
  fun setConst (K idx) v = Array.update (vars, idx + 250, v)
  fun getConst (K idx) = Array.sub (vars, idx + 250)

  fun run insns =
    case insns of
      [] => ()
    | (Move arg :: rest) => move arg rest
    | (Load arg :: rest) => load arg rest
    | (Arith arg :: rest) => arith arg rest
    | (Unary (Not, dest, src) :: rest) => not (dest, src) rest
    | (Unary (Minus, dest, src) :: rest) => minus (dest, src) rest

  and move {dest, src} = (setReg dest (getReg src); run)
  and load (dest, source) = 
    let
      val value =
        case source of
          Left (F f) => V.Number f
        | Left (B b) => V.Boolean b
        | Left Nil => V.Nil
        | Right const => getConst const
    in
      (setReg dest value; run)
    end
  and arith (oper, dest, l, r) =
    let
      val oper =
        case oper of
          Add => op+
        | Sub => op-
        | Mul => op*
        | Div => op/

      fun fetchNum loc =
        let
          val v =
            case loc of
              Right const => getConst const
            | Left reg => getReg reg
        in
          case v of
            V.Number n => n
          | _ => raise Fail "type error"
        end
    in
      ((setReg dest o V.Number o oper) (fetchNum l, fetchNum r); run)
    end
  and not (dest, src) =
    let
      val src = 
        case getReg src of
          V.Boolean b => b
        | _ => raise Fail "type error"
    in
      ((setReg dest o V.Boolean o Bool.not) src; run)
    end
  and minus (dest, src) =
    let
      val src = 
        case getReg src of
          V.Number r => r
        | _ => raise Fail "type error"
    in
      ((setReg dest o V.Number o op~) src; run)
    end

  val insns =
    [ Move {dest = R 0, src = R 1}
    , Load (R 1, Left (F 1.0))
    , Load (R 2, Left (F 1000.0))
    , Arith (Add, R 3, Left (R 1), Left (R 2))
    ]

  (* val _ = run insns *)
  (* val _ = sparseDump () *)
end
