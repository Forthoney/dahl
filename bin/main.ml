let () = Dahl.Lexer.lex stdin |> Seq.map Dahl.Lexer.tok_to_string |> Seq.iter print_endline
