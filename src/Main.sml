datatype stop_at = Lex | Parse | All
val until = ref All
val src = ref TextIO.stdIn

val parseOnly =
  { usage = {name = "parse-only", desc = "Parse only"}
  , arg = Argument.None (fn _ => until := Parse)
  }
val lexOnly =
  { usage = {name = "lex-only", desc = "Lex only"}
  , arg = Argument.None (fn _ => until := Lex)
  }

fun getScript arg =
  case arg of
    SOME "-" | NONE => TextIO.stdIn
  | SOME script => TextIO.openIn script

structure Command =
  CommandFn
    (structure Parser = Parser_PrefixFn(val prefix = "--")
     type action = unit
     val desc = "A Lua implementation in SML"
     val flags = [parseOnly, lexOnly]
     val anonymous = Argument.Optional
       { metavar = "SCRIPT"
       , action =
          (fn (SOME "-" | NONE) => ()
            | (SOME script) => src := (TextIO.openIn script))
       })

val debugTokenScan = Reader.inspect (fn tok => print (Lexer.toString tok ^ "\n")) Lexer.scan 
structure Parser = ParserFn(type strm = TextIO.StreamIO.instream val tokenScan = debugTokenScan)

val _ = Command.run (CommandLine.arguments ())
val _ =
  case ! until of
    Lex => (Reader.app (fn tok => print (Lexer.toString tok ^ "\n")) Lexer.scan o TextIO.getInstream o !) src
  | Parse => (Reader.app (print o SExp.toString o AST.SExpFrom.block) Parser.scan o TextIO.getInstream o !) src
  | All => Interpreter.run' () 
