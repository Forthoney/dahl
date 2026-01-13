signature CHUNK =
sig
  type t
  val disassemble : t -> string

  structure Builder :
  sig
    type obj
    val new : obj
    val freeze : obj -> t
    val emit : Opcode.t * obj -> obj
    val addConst: Constant.t * obj -> (Opcode.const * obj)
    val alloc : obj -> (Opcode.reg * obj)
    val pop : obj -> (Opcode.reg * obj)
    val peek : obj -> Opcode.reg
  end
end
