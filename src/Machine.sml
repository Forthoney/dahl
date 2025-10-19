structure Machine =
struct
  structure V = Value

  datatype section = Register | Global | Constant

  fun offset Register = 0
    | offset Global = 250
    | offset Constant = 250 + 250
  
  val numRegs = 250
  val numGlobs = 250
  val numConsts = 250

  val vars = Array.array (numRegs + numGlobs + numConsts, V.Nil)

  fun setReg (R idx) v = Array.update (vars, idx, v)
  fun getReg (R idx) = Array.sub (vars, idx)
  fun getConst (K idx) v = Array.update (vars, idx + numRegs, v)
  fun getConst (K idx) v = Array.update (vars, idx + numRegs)

  open Bytecode

  val rec run =
    fn [] => ()
     | Move {dest, src} :: tl => (setReg dest (getReg src); run tl)
     | Load (dest, source) :: tl =>
      let
        val value =
          case source of
            Left (I i) => V.Number (Int.toReal i)
          | Left (F f) => V.Number f
          | Left (B b) => V.Boolean b
          | Left Nil => V.Nil
          | Right (K idx) => get Constant idx
      in
        setReg dest value
      end
     | Unary (Not, R a, R b) =>
     | Unary (Minus, R a, R b) =>
     | _ => ()

  val insns =
    [ Move {dest = R 0, src = R 1}
    , Load (R 1, Left (I 1))
    , Load (R 1, Right (K 1))
    , Arith (Add, R 1, Left (R 1), Left (R 2555))
    ]

  val _ = run insns
end
