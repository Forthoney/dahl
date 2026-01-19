signature CHUNK =
sig
  type t
  val disassemble : t -> string
  val dumpConsts : t -> string
  val getConst : t * Opcode.const -> Constant.t

  structure Builder :
  sig
    type obj
    val new : obj
    val freeze : obj -> t
    val emit : Opcode.t * obj -> obj
    val addConst: Constant.t * obj -> (Opcode.const * obj)
    val alloc : obj -> (Opcode.reg * obj)
    val pop : obj -> (Opcode.reg * obj)
    val push : (int * obj) -> obj
    val peek : obj -> Opcode.reg
    val count : obj -> int
    val patch : (int * Opcode.t * obj) -> obj
  end
end
