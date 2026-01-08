fun main () =
  let
    val args = CommandLine.arguments ()
    val ast =
      case args of
        [] => Driver.parseStdIn ()
      | ["-"] => Driver.parseStdIn ()
      | filename :: _ => Driver.parseFile filename
  in
    (print o Compiler.toDebug o Compiler.compile) ast
  end
  handle
    Fail msg =>
      (print ("\nError: " ^ msg ^ "\n"); OS.Process.exit OS.Process.failure)
  | IO.Io {cause, function, name} =>
      (print ("\nIO Error: " ^ name ^ "\n"); OS.Process.exit OS.Process.failure)
  | _ =>
      ( print "\nAn unknown error occurred.\n"
      ; OS.Process.exit OS.Process.failure
      )

val _ = main ()
