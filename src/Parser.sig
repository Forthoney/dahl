signature PARSER =
sig
  val scan: (AST.block, TextIO.StreamIO.instream) StringCvt.reader
end
