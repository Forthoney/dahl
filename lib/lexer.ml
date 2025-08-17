type token =
  | Plus
  | Minus
  | Mult
  | Div
  | Mod
  | Carat
  | Colon
  | Semicolon
  | Comma
  | Hash
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBrack
  | RBrack
  | Dash
  | Tilde
  | Assign
  | Dot
  | Concat
  | Dots
  | Eq
  | Ge
  | Geq
  | Le
  | Leq
  | Neq
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
  | Number of string
  | Str of string
  | Name of string

let tok_to_string = function
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Mult -> "Mult"
  | Div -> "Div"
  | Mod -> "Mod"
  | Carat -> "Carat"
  | Colon -> "Colon"
  | Semicolon -> "Semicolon"
  | Comma -> "Comma"
  | Hash -> "Hash"
  | LParen -> "LParen"
  | RParen -> "RParen"
  | LBrace -> "LBrace"
  | RBrace -> "RBrace"
  | LBrack -> "LBrack"
  | RBrack -> "RBrack"
  | Dash -> "Dash"
  | Tilde -> "Tilde"
  | Assign -> "Assign"
  | Dot -> "Dot"
  | Concat -> "Concat"
  | Dots -> "Dots"
  | Eq -> "Eq"
  | Ge -> "Ge"
  | Geq -> "Geq"
  | Le -> "Le"
  | Leq -> "Leq"
  | Neq -> "Neq"
  | And -> "And"
  | Break -> "Break"
  | Do -> "Do"
  | Else -> "Else"
  | Elseif -> "Elseif"
  | End -> "End"
  | False -> "False"
  | For -> "For"
  | Function -> "Function"
  | If -> "If"
  | In -> "In"
  | Local -> "Local"
  | Nil -> "Nil"
  | Not -> "Not"
  | Or -> "Or"
  | Repeat -> "Repeat"
  | Return -> "Return"
  | Then -> "Then"
  | True -> "True"
  | Until -> "Until"
  | While -> "While"
  | Number n -> "Number " ^ n
  | Str s -> "Str " ^ s
  | Name s -> "Name " ^  s

let input_char' ic =
  match input_char ic with c -> This c | exception End_of_file -> Null

let null_bind f = function Null -> Null | This v -> f v

let is_digit c = '0' <= c && c <= '9'
let is_alpha c = 'A' <= c && c <= 'z'

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
          | c when is_digit c ->
              let rec escape_digit acc = function
                | 2 -> Char.chr acc |> add_and_next
                | i -> (
                    match input_char ic with
                    | c when is_digit c ->
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
    let emit tok = This (tok, (lineno, input_char' ic)) in
    null_bind
      (function
        | '+' -> emit Plus
        | '-' -> emit Minus
        | '*' -> emit Mult
        | '/' -> emit Div
        | '%' -> emit Mod
        | '^' -> emit Carat
        | ':' -> emit Colon
        | ';' -> emit Semicolon
        | ',' -> emit Comma
        | '#' -> emit Hash
        | '(' -> emit LParen
        | ')' -> emit RParen
        | '{' -> emit LBrace
        | '}' -> emit RBrace
        | ']' -> emit RBrack
        | '_' -> emit (Name "_")
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
        | '.' ->
          (match input_char' ic with
          | This '.' ->
              (match input_char' ic with
                | This '.' -> emit Dots
                | next -> This (Concat, (lineno, next)))
          | This c when is_digit c -> emit (Number "todo")
          | next -> This (Dot, (lineno, next)))
        | ' ' | '\t' | '\011' | '\012'  -> aux (lineno, input_char' ic)
        | c when is_digit c -> emit (Number "todo") 
        | c when is_alpha c -> 
          let buf = Buffer.create 8 in
          let () = Buffer.add_char buf c in
          let rec aux = function
            | This c when is_alpha c || is_digit c -> Buffer.add_char buf c; input_char' ic |> aux
            | _ -> Buffer.contents buf
          in
          (match input_char' ic |> aux with
          | "and" -> emit And
          | "break" -> emit Break
          | "do" -> emit Do
          | "else" -> emit Else
          | "elseif" -> emit Elseif
          | "end" -> emit End
          | "false" -> emit False
          | "for" -> emit For
          | "function" -> emit Function
          | "if" -> emit If
          | "in" -> emit In
          | "local" -> emit Local
          | "nil" -> emit Nil
          | "not" -> emit Not
          | "or" -> emit Or
          | "repeat" -> emit Repeat
          | "return" -> emit Return
          | "then" -> emit Then
          | "true" -> emit True
          | "until" -> emit Until
          | "while" -> emit While
          | nonreserved -> emit (Name nonreserved))
        | _ -> failwith "unknown char")
      c
  in
  null_unfold aux (0, input_char' ic)
