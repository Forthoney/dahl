let () =
  let lexbuf = Lexing.from_channel stdin in
  let tok = ref (Dahl.Lexer.token lexbuf) in
  while !tok != Dahl.Token.EOF do
    print_endline (Dahl.Token.to_string !tok);
    tok := Dahl.Lexer.token lexbuf
  done
