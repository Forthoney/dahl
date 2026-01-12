structure Opcode =
struct
  datatype reg = R of int
  datatype const = K of int

  datatype t =
    RET of reg * reg
  | LOAD_NIL of reg
  | LOAD_TRUE of reg
  | LOAD_FALSE of reg
  | LOAD of reg * const
  | NEG of reg * reg
  | NOT of reg * reg
  | ADD of reg * reg * reg
  | SUB of reg * reg * reg
  | MUL of reg * reg * reg
  | DIV of reg * reg * reg
  | MOD of reg * reg * reg
  | POW of reg * reg * reg
  | EQ of reg * reg * reg
  | NE of reg * reg * reg
  | LE of reg * reg * reg
  | LT of reg * reg * reg
  | GE of reg * reg * reg
  | GT of reg * reg * reg

  fun disassemble c =
    let
      fun reg (R r) = "R[" ^ Int.toString r ^ "]"
      fun const (K k) = "K[" ^ Int.toString k ^ "]"
      fun unary name (d, s) = [name, reg d, reg s]
      fun binary name (d, l, r) = [name, reg d, reg l, reg r]
      val fmt = 
        case c of
          RET (from, to) => ["RET", reg from, reg to]
        | LOAD (dest, from) => ["LOAD", reg dest, const from]
        | LOAD_NIL dest => ["LOAD_NIL", reg dest]
        | LOAD_TRUE dest => ["LOAD_TRUE", reg dest]
        | LOAD_FALSE dest => ["LOAD_FALSE", reg dest]
        | NOT opr => unary "NOT" opr
        | NEG opr => unary "NEG" opr
        | ADD opr => binary "ADD" opr
        | SUB opr => binary "SUB" opr
        | MUL opr => binary "MUL" opr
        | DIV opr => binary "DIV" opr
        | MOD opr => binary "MOD" opr
        | POW opr => binary "POW" opr
        | LE opr => binary "LE" opr
        | LT opr => binary "LT" opr
        | GE opr => binary "GE" opr
        | GT opr => binary "GT" opr
        | EQ opr => binary "EQ" opr
        | NE opr => binary "NE" opr
    in
      String.concatWith "\t" fmt
    end
end
