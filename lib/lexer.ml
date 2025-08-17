type token =
  | Dash
  | Tilde
  | LBrack
  | RBrack
  | Assign
  | And
  | Break
  | Do
  | Else
  | Elseif
  | End
  | False
  | For
  | Function
  | If
  | In
  | Local
  | Nil
  | Not
  | Or
  | Repeat
  | Return
  | Then
  | True
  | Until
  | While
  | Concat
  | Dots
  | Eq
  | Ge
  | Geq
  | Le
  | Leq
  | Neq
  | Number of string
  | Name of string
  | Str of string
  | Eos

let lex ic =
  let rec aux lineno c =
    let add_tok ?(line_incr=0) tok =
      match input_char ic with
      | c -> fun () -> Seq.Cons (tok, aux (lineno + line_incr) c)
      | exception End_of_file -> Seq.return tok
    in
    let cmp_ops eq no_eq =
      match input_char ic with
      | '=' -> add_tok eq
      | c -> fun () -> Seq.Cons (no_eq, aux lineno c)
      | exception End_of_file -> Seq.return no_eq
    in
    let read_string delim =
      let buf = Buffer.create 10 in
      let rec aux' line_incr c =
        try 
          match c with
          | c when c = delim -> (Buffer.contents buf, line_incr)
          | '\n' | '\r' -> failwith "unfinished string"
          | '\\' -> (
              let c = input_char ic in
              let curr, next, line_incr =
                match c with
                | 'a' -> ('\007', Null, 0)
                | 'b' -> ('\b', Null, 0)
                | 'f' -> ('\012', Null, 0)
                | 'n' -> ('\n', Null, 0)
                | 'r' -> ('\r', Null, 0)
                | 't' -> ('\t', Null, 0)
                | 'v' -> ('\011', Null, 0)
                | '\n' | '\r' -> ('\n', Null, 1)
                | '0' .. '9' ->
                    let rec escape_digit acc = function
                      | 2 -> (Char.chr acc, Null, 0)
                      | i -> (
                          let c' = input_char ic in
                          match c' with
                          | '0' .. '9' ->
                              escape_digit
                                ((acc * 10) + Char.code c' - Char.code '0')
                                (i + 1)
                          | c' -> (Char.chr acc, This c', 0))
                    in
                    escape_digit (Char.code c - Char.code '0') 0
                | _ -> failwith "todo"
              in
              Buffer.add_char buf curr;
              match next with
              | This next -> aux' line_incr next
              | Null -> input_char ic |> aux' line_incr)
          | c ->
              Buffer.add_char buf c;
              input_char ic |> aux' line_incr
        with End_of_file -> failwith "String term"
      in
      let tok, line_incr = input_char ic |> aux' 0 in
      Str tok |> add_tok ~line_incr
    in
    match c with
    | '\r' -> (
        try
          match input_char ic with
          | '\n' -> input_char ic |> aux (lineno + 1)
          | c -> aux (lineno + 1) c
        with End_of_file -> Seq.empty)
    | '\n' -> (
        try
          match input_char ic with
          | '\r' -> input_char ic |> aux (lineno + 1)
          | c -> aux (lineno + 1) c
        with End_of_file -> Seq.empty)
    | '=' -> cmp_ops Eq Assign
    | '<' -> cmp_ops Leq Le
    | '>' -> cmp_ops Geq Ge
    | '~' -> cmp_ops Neq Tilde
    | '"' -> read_string '"'
    | '\'' -> read_string '\''
    (* | '.' -> *)
      
    | _ -> failwith "todo"
  in
  aux 0
