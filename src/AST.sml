structure AST =
struct
  datatype binop =
    Plus
  | Minus
  | Times
  | Div
  | Mod
  | Pow
  | Concat
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or

  datatype unop = Neg | Not | Len

  (* AST Structure *)
  datatype exp =
    Nil
  | Bool of bool
  | Number of real
  | String of string
  | VarArg
  | Func of func_body
  | Prefix of prefix_exp
  | TableCons of field list
  | BinOp of binop * exp * exp
  | UnOp of unop * exp

  and prefix_exp =
    PVar of var
  | PCall of call
  | PParen of exp

  and var =
    Name of string
  | Index of prefix_exp * exp (* t[k] *)
  | Prop of prefix_exp * string (* t.k *)

  and call =
    Call of prefix_exp * exp list
  | MethodCall of prefix_exp * string * exp list

  and field =
    FieldKey of exp * exp
  | FieldProp of string * exp
  | FieldSeq of exp

  and func_body =
    FuncBody of (string list * bool) * block

  and stmt =
    Assign of var list * exp list
  | LocalAssign of string list * exp list
  | CallStmt of call
  | Do of block
  | While of exp * block
  | Repeat of block * exp
  | If of (exp * block) list * block option
  | NumFor of string * exp * exp * exp option * block
  | GenFor of string list * exp list * block
  | FuncDef of func_name * func_body
  | LocalFunc of string * func_body
  | Break
  | Return of exp list

  and func_name =
    FuncName of string list * string option

  and block =
    Block of stmt list
end
