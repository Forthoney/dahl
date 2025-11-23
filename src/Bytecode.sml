structure Bytecode =
struct
  type register = int
  type constant = int

  datatype ('a, 'b) either =
    A of 'a
  | B of 'b

  datatype opcode =
    Move of register * register (* R[A] := R[B] *)
  | Load of register * constant
  | Add of register * register
  | NewTable of register * {arr: int, hash: int}
  | SetTable of {table: register, key: register, val: register}

  fun opToString opc =
    case opc of
      MOVE (a, b) => "MOVE " ^ Int.toString a ^ " " ^ Int.toString b
    | LOADK (a, bx) => "LOADK " ^ Int.toString a ^ " " ^ Int.toString bx
    | ADD (a, _, _) => "ADD " ^ Int.toString a ^ "..."
    | CALL (a, b, c) =>
        "CALL " ^ Int.toString a ^ " " ^ Int.toString b ^ " " ^ Int.toString c
    | RETURN (a, b) => "RETURN " ^ Int.toString a ^ " " ^ Int.toString b
    | JMP (sbx) => "JMP " ^ Int.toString sbx
    | EQ _ => "EQ..."
    | LT _ => "LT..."
    | LE _ => "LE..."
    | TEST _ => "TEST..."
    | GETGLOBAL (a, bx) => "GETGLOBAL " ^ Int.toString a ^ " " ^ Int.toString bx
    | CLOSURE (a, bx) => "CLOSURE " ^ Int.toString a ^ " " ^ Int.toString bx
    | TAILCALL (a, b) => "TAILCALL " ^ Int.toString a ^ " " ^ Int.toString b
    | _ => "OP..."

end
