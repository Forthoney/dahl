(* val _ = *)
  (* case CommandLine.arguments () of *)
    (* [] => run TextIO.stdIn *)
  (* | [fileName] => run (TextIO.openIn fileName) *)
  (* | _ => raise Fail "invalid usage" *)

(* val chunk = Chunk.compile () *)
(* val _ = print (Chunk.disassemble chunk ^ "\n") *)
(* val _ = ArraySlice.app (print o Value.toString) (Machine.interpret chunk) *)
