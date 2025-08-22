(*
The MIT License
Copyright (c) 2020 Jason D. Nielsen <drjdnielsen@gmail.com>
Copyright (c) 2025 Seong-Heon Jung <castlehoneyjung@gmail.com>
 *)

type t =
  | Break
  | Ellipsis
  | Ident of string
  | Number of string
  | Bool of string
  | String of string
  | Args of t
  | Assign of t * t
  | Binop of string * t * t
  | Do of t
  | Elseif of t * t
  | Fassign of t * t
  | Fbody of t * t
  | For1 of t * t * t * t
  | For2 of t * t * t * t * t
  | Forin of t * t * t
  | Function of t * t
  | FunctionE of t
  | Goto of t
  | If1 of t * t
  | If2 of t * t * t
  | If3 of t * t * t
  | If4 of t * t * t * t
  | Key1 of t
  | Key2 of t
  | Label of t
  | Lassign of t * t
  | Lfunction of t * t
  | Lnames of t
  | Member of t * t
  | Mcall of t * t * t
  | Pexp of t
  | Repeat of t * t
  | Return of t
  | Table of t
  | Unop of string * t
  | Vargs of t
  | While of t * t
  | Clist of t list
  | Elist of t list
  | FNlist of t list
  | Slist of t list

let extract_list = function
  | Clist i -> i
  | Elist i -> i
  | FNlist i -> i
  | Slist i -> i
  | _ -> failwith "Not a valid t list!"
