fun run rawStrm =
  let
    val rdr = Lexer.run (Stream.Char.flatten TextIO.StreamIO.inputLine)
    val strm = (Lexer.mk o Stream.Char.mk o TextIO.getInstream) rawStrm
    val chunk = Compiler.run rdr strm
  in
    print (Chunk.disassemble chunk ^ "\n")
  end

val _ =
  case CommandLine.arguments () of
    [] => run TextIO.stdIn
  | [fileName] => run (TextIO.openIn fileName)
  | _ => raise Fail "invalid usage"

(* val chunk = Chunk.compile () *)
(* val _ = print (Chunk.disassemble chunk ^ "\n") *)
(* val _ = ArraySlice.app (print o Value.toString) (Machine.interpret chunk) *)
