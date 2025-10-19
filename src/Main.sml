val debugTokenScan = Reader.inspect (fn tok => print (Lexer.toString tok ^ "\n")) Lexer.scan 
structure Parser = ParserFn(type strm = TextIO.StreamIO.instream val tokenScan = debugTokenScan)
val stdIn = TextIO.getInstream TextIO.stdIn
val _ = Reader.app (print o SExp.toString o AST.blockToSExp) Parser.scan stdIn
