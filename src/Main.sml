fun run rawStrm =
  let
    val rdr = Stream.Char.flatten TextIO.StreamIO.inputLine
    val st = (Lexer.mk o Stream.Char.mk o TextIO.getInstream) rawStrm
  in
    Stream.force (Stream.app (fn t => print (Lexer.tokenToString t ^ "\n")) (Lexer.run rdr)) st
  end

val _ =
  case CommandLine.arguments () of
    [] => run TextIO.stdIn
  | [fileName] => run (TextIO.openIn fileName)
  | _ => raise Fail "invalid usage"

(* val chunk = Chunk.compile () *)
(* val _ = print (Chunk.disassemble chunk ^ "\n") *)
(* val _ = ArraySlice.app (print o Value.toString) (Machine.interpret chunk) *)
