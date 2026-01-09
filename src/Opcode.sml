structure Opcode =
struct
  datatype reg = R of int
  datatype const = K of int

  datatype t =
    RET of reg * reg
  | LOAD of reg * const
  | NEG of reg * reg
  | ADD of reg * reg * reg
  | SUB of reg * reg * reg
  | MUL of reg * reg * reg
  | DIV of reg * reg * reg
  | MOD of reg * reg * reg
  | POW of reg * reg * reg

  fun disassemble c =
    let
      fun reg (R r) = "R[" ^ Int.toString r ^ "]"
      fun const (K k) = "K[" ^ Int.toString k ^ "]"
    in
      case c of
        RET (from, to) => String.concatWith " " ["RET", reg from, reg to]
      | LOAD (dest, from) => String.concatWith " " ["LOAD", reg dest, const from]
      | ADD (dest, l, r) => String.concatWith " " ["ADD", reg dest, reg l, reg r]
      | SUB (dest, l, r) => String.concatWith " " ["SUB", reg dest, reg l, reg r]
      | MUL (dest, l, r) => String.concatWith " " ["MUL", reg dest, reg l, reg r]
      | DIV (dest, l, r) => String.concatWith " " ["DIV", reg dest, reg l, reg r]
      | MOD (dest, l, r) => String.concatWith " " ["MOD", reg dest, reg l, reg r]
      | POW (dest, l, r) => String.concatWith " " ["POW", reg dest, reg l, reg r]
    end
end
