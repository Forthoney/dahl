structure Bytecode =
struct
  type register = int
  type constant = int

  datatype opcode =
    Move of register * register (* R[A] := R[B] *)
  | LoadNil of register
  | LoadBool of register * bool
  | Load of register * constant
  | Not of register * register
  | Neg of register * register
  | Len of register * register
  | Plus of register * register * register
  | Minus of register * register * register
  | Times of register * register * register
  | Div of register * register * register
  | Mod of register * register * register
  | Pow of register * register * register
  | Concat of register * register * register
  | Eq of register * register * register
  | Neq of register * register * register
  | Lt of register * register * register
  | Le of register * register * register
  | Gt of register * register * register
  | Ge of register * register * register
  | And of register * register * register
  | Or of register * register * register

  fun toString opc =
    let
      fun binReg name (a, b) =
        String.concatWith " " [name, Int.toString a, Int.toString b]

      fun triReg name (a, b, c) =
        String.concatWith " " [name, Int.toString a, Int.toString b, Int.toString c]
    in
      case opc of
        Move ab => binReg "Move" ab
      | LoadNil r => "LoadNil " ^ Int.toString r
      | LoadBool (r, b) => String.concatWith " " ["LoadBool", Int.toString r, Bool.toString b]
      | Load (r, k) => "Load " ^ Int.toString r ^ " K[" ^ Int.toString k ^ "]"
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
