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

let input_char' ic =
  match input_char ic with c -> This c | exception End_of_file -> Null

let null_bind f = function Null -> Null | This v -> f v

let rec null_unfold f x () =
  match f x with
  | Null -> Seq.Nil
  | This (x, seq) -> Seq.Cons (x, null_unfold f seq)

let lex ic =
  let rec aux (lineno, c) =
    let cmp_ops eq no_eq =
      match input_char' ic with
      | This '=' -> This (eq, (lineno, input_char' ic))
      | otherwise -> This (no_eq, (lineno, otherwise))
    in
    let read_string delim =
      let buf = Buffer.create 10 in
      let rec build (lineno : int) =
        let escape () =
          let add_and_next c =
            Buffer.add_char buf c;
            input_char ic |> build lineno
          in
          match input_char ic with
          | 'a' -> add_and_next '\007'
          | 'b' -> add_and_next '\b'
          | 'f' -> add_and_next '\012'
          | 'n' -> add_and_next '\n'
          | 'r' -> add_and_next '\r'
          | 't' -> add_and_next '\t'
          | 'v' -> add_and_next '\011'
          | '\n' | '\r' ->
              Buffer.add_char buf '\n';
              input_char ic |> build (lineno + 1)
          | c when '0' <= c && c <= '9' ->
              let rec escape_digit acc = function
                | 2 -> Char.chr acc |> add_and_next
                | i -> (
                    match input_char ic with
                    | c when '0' <= c && c <= '9' ->
                        escape_digit
                          ((acc * 10) + Char.code c - Char.code '0')
                          (i + 1)
                    | c ->
                        Char.chr acc |> Buffer.add_char buf;
                        build lineno c)
              in
              escape_digit (Char.code c - Char.code '0') 0
          | _ | (exception End_of_file) -> failwith "invalid escape"
        in
        function
        | c when c = delim -> (Buffer.contents buf, lineno)
        | '\n' | '\r' -> failwith "unterminated string"
        | '\\' -> escape ()
        | c ->
            Buffer.add_char buf c;
            input_char ic |> build lineno
      in
      try
        let s, lineno = input_char ic |> build lineno in
        This (Str s, (lineno, input_char' ic))
      with End_of_file -> failwith "unterminated string"
    in
    null_bind
      (function
        | '\r' -> (
            match input_char' ic with
            | This '\n' -> aux (lineno + 1, input_char' ic)
            | otherwise -> aux (lineno + 1, otherwise))
        | '\n' -> (
            match input_char' ic with
            | This '\r' -> aux (lineno + 1, input_char' ic)
            | otherwise -> aux (lineno + 1, otherwise))
        | '=' -> cmp_ops Eq Assign
        | '<' -> cmp_ops Leq Le
        | '>' -> cmp_ops Geq Ge
        | '~' -> cmp_ops Neq Tilde
        | '"' -> read_string '"'
        | '\'' -> read_string '\''
        | _ -> failwith "todo")
      c
  in
  null_unfold aux (0, input_char' ic)
