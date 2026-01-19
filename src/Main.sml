fun run rawStrm =
  let
    val rdr = Lexer.run (Stream.Char.flatten TextIO.StreamIO.inputLine)
    val strm = (Lexer.mk o Stream.Char.mk o TextIO.getInstream) rawStrm
    val chunk = Compiler.run rdr strm
  in
      let
        val state = Machine.interpret chunk
      in
        ( print (Chunk.disassemble chunk ^ "\n")
        ; print ("Constants\n" ^ Chunk.dumpConsts chunk ^ "\n")
        ; print ("Globals\n" ^ Machine.dumpGlobals state ^ "\n")
        ; print ("Registers\n" ^ Machine.dump (#regs state) ^ "\n")
        )
      end
  end

val _ =
  case CommandLine.arguments () of
    [] => run TextIO.stdIn
  | [fileName] => run (TextIO.openIn fileName)
  | _ => raise Fail "invalid usage"
