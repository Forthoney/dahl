structure LuaLrVals = LuaLrValsFun (structure Token = LrParser.Token)
structure LuaLex = LuaLexFun (structure Tokens = LuaLrVals.Tokens)
structure LuaParser =
  Join
    (structure ParserData = LuaLrVals.ParserData
     structure Lex = LuaLex
     structure LrParser = LrParser)

structure Driver =
struct

  (* Generic helper to parse from any TextIO stream (File or Stdin) *)
  fun parseStream stream =
    let
      (* The lexer requests 'n' characters. We read up to 'n' from the stream. *)
      fun readFunc n = TextIO.inputN (stream, n)
      val lexer = LuaParser.makeLexer readFunc

      fun printError (s, p1, p2) =
        print
          ("Syntax Error: " ^ s ^ " between line " ^ Int.toString p1 ^ " and "
           ^ Int.toString p2 ^ "\n")

      val (result, _) = LuaParser.parse (0, lexer, printError, ())
    in
      result
    end
    handle LuaParser.ParseError => raise Fail "Parser Failure"

  (* Helper to parse a string directly *)
  fun parseString str =
    let
      val pos = ref 0
      val len = String.size str
      fun readFunc n =
        if !pos >= len then
          ""
        else
          let
            val remaining = len - !pos
            val chunkLen = if n < remaining then n else remaining
            val chunk = String.substring (str, !pos, chunkLen)
          in
            pos := !pos + chunkLen;
            chunk
          end

      val lexer = LuaParser.makeLexer readFunc
      fun printError (s, p1, p2) =
        print ("Error: " ^ s ^ "\n")
      val (result, _) = LuaParser.parse (15, lexer, printError, ())
    in
      result
    end
    handle LuaParser.ParseError => raise Fail "Parser Failure"

  (* Parse from a specific file path *)
  fun parseFile filename =
    let
      val file = TextIO.openIn filename
      val result = parseStream file
    in
      TextIO.closeIn file;
      result
    end
    handle LuaParser.ParseError =>
      ( TextIO.closeIn (TextIO.openIn filename) handle _ => ()
      ; raise Fail "Parsing failed."
      )

  (* Parse from Standard Input *)
  fun parseStdIn () = parseStream TextIO.stdIn

end
