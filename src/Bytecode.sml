structure Bytecode =
struct
  datatype ('a, 'b) either = Left of 'a | Right of 'b

  datatype register = R of int
  datatype constant = K of int
  datatype immediate = I of int | F of real | B of bool | Nil

  datatype arith_op = Add | Sub | Mul | Div | Pow
  datatype cmp_op = Eq | Lt | Le
  datatype un_op = Not | Minus

  type rk = (register, constant) either

  datatype t =
    Move of {dest: register, src: register}
  | Load of register * (immediate, constant) either
  | Arith of arith_op * register * rk * rk
  | Unary of un_op * register * register
  | NewTable of register * int * int
  | SetTable of register * rk * rk
  | GetTable of register * rk * rk
  | SetGlobal of register * constant
  | GetGlobal of register * constant
  | Compare of cmp_op * int * rk * rk
  | Return of {start: register, offset: int}
end
