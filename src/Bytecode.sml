structure Bytecode =
struct
  datatype reg = R of int
  datatype const = K of int
  datatype rk = R_ of reg | K_ of const

  datatype t =
    Move of reg * reg
  | Load of reg * const
  | LoadNil of reg
  | LoadBool of reg * bool
  | Not of reg * reg
  | Neg of reg * reg
  | Len of reg * reg
  | Plus of reg * rk * rk
  | Minus of reg * rk * rk
  | Times of reg * rk * rk
  | Div of reg * rk * rk
  | Mod of reg * rk * rk
  | Pow of reg * rk * rk
  | Concat of reg * rk * rk
  | Eq of reg * rk * rk
  | Neq of reg * rk * rk
  | Lt of reg * rk * rk
  | Le of reg * rk * rk
  | Gt of reg * rk * rk
  | Ge of reg * rk * rk
  | And of reg * rk * rk
  | Or of reg * rk * rk
  | NewTable of reg * int * int
  | SetTable of reg * rk * rk

  fun toString opc =
    let
      fun regToString (R r) = "R[" ^ Int.toString r ^ "]"
      fun constToString (K r) = "K[" ^ Int.toString r ^ "]"
      fun rkToString (R_ r) = regToString r
        | rkToString (K_ k) = constToString k

      fun binReg name (a, b) =
        String.concatWith " " [name, regToString a, regToString b]

      fun triReg name (a, b, c) =
        String.concatWith " " [name, regToString a, rkToString b, rkToString c]
    in
      case opc of
        Move ab => binReg "Move" ab
      | LoadNil r => "LoadNil " ^ regToString r
      | LoadBool (r, b) => String.concatWith " " ["LoadBool", regToString r, Bool.toString b]
      | Load (r, k) => String.concatWith " " ["Load", regToString r, constToString k]
      | Not ab => binReg "Not" ab
      | Neg ab => binReg "Neg" ab
      | Len ab => binReg "Len" ab
      | Plus abc => triReg "Plus" abc
      | Minus abc => triReg "Minus" abc
      | Times abc => triReg "Times" abc
      | Div abc => triReg "Div" abc
      | Mod abc => triReg "Mod" abc
      | Pow abc => triReg "Pow" abc
      | Concat abc => triReg "Concat" abc
      | Eq abc => triReg "Eq" abc
      | Neq abc => triReg "Neq" abc
      | Lt abc => triReg "Lt" abc
      | Le abc => triReg "Le" abc
      | Gt abc => triReg "Gt" abc
      | Ge abc => triReg "Ge" abc
      | And abc => triReg "And" abc
      | Or abc => triReg "Or" abc
      | NewTable (a, arr, tbl) => String.concatWith " " ["NewTable", regToString a, Int.toString arr, Int.toString tbl]
      | SetTable abc => triReg "NewTable" abc
    end

  fun fromUnaryOp unop =
    case unop of
      AST.Not => Not
    | AST.Neg => Neg
    | AST.Len => Len

  fun fromBinaryOp binop =
    case binop of
      AST.Plus => Plus
    | AST.Minus => Minus 
    | AST.Times => Times 
    | AST.Div => Div 
    | AST.Mod => Mod 
    | AST.Pow => Pow 
    | AST.Concat => Concat 
    | AST.Eq => Eq 
    | AST.Neq => Neq 
    | AST.Lt => Lt 
    | AST.Le => Le 
    | AST.Gt => Gt 
    | AST.Ge => Ge 
    | AST.And => And 
    | AST.Or => Or 
end
