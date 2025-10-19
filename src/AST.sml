structure AST =
struct
  type pos = int

  datatype bin_op =
    Plus
  | Minus
  | Mult
  | Div
  | Pow
  | Mod
  | Concat
  | Le
  | Leq
  | Ge
  | Geq
  | Eq
  | Neq
  | And
  | Or

  datatype un_op = Neg | Not | Length

  type name = string
  type func_name = {names: name list, method: name option}

  datatype exp =
    ENil
  | EFalse
  | ETrue
  | ENumber of string
  | EString of string
  | EVararg

  datatype laststat = Return of exp list | Break

  (* "hi":foo():bar() *)
  (* Method (Method (PExp "hi", foo, []) , bar, []) *)
  datatype var =
    VName of name
  | VIndex of prefix_exp * exp
  | VProp of prefix_exp * name
  and prefix_exp =
    PVar of var
  | PFnCall of fn_call
  | PExp of exp
  and fn_call =
    Function of prefix_exp * args
  | Method of prefix_exp * name * args
  and args =
    AExp of exp list
  | ATable of field list
  | AString of string
  and field =
    FExp of exp * exp
  | FLiteral of name * exp
  | FArray of exp
  and stat =
    Assign of name list * exp list
  | LocalAssign of name list * exp list
  | LocalFunction of name * {params: name list, vararg: bool, block: block}
  and block =
    Block of stat list * laststat option

  structure S = SExp

  val expToSExp =
    fn ENil => S.Atom "ENil"
     | EFalse => S.Atom "EFalse"
     | ETrue => S.Atom "ETrue"
     | ENumber s => S.List [S.Atom "ENumber", S.Atom s]
     | EString s => S.List [S.Atom "EString", S.Atom ("\"" ^ String.toString s ^ "\"")]
     | EVararg => S.Atom "EVararg"


  fun stmtToSExp node =
    case node of
      LocalAssign (names, exps) =>
        S.namedStruct ("LocalAssign", [S.List (map S.Atom names), S.List (map expToSExp exps)])
    | LocalFunction (name, {params, vararg, block}) =>
      let
        val vararg = if vararg then [S.Atom "..."] else [] 
      in
        S.namedStruct ("LocalFunction", [S.List (map S.Atom params @ vararg), blockToSExp block])
      end
  and laststmtToSExp (Return exps) = S.namedStruct ("Return", map expToSExp exps)
    | laststmtToSExp (Break) = S.Atom "Break"
  and blockToSExp (Block (stmts, last)) =
    let
      val l = case last of SOME ls => [laststmtToSExp ls] | NONE => []
    in
      S.namedStruct ("Block", S.List (map stmtToSExp stmts) :: l)
    end
end
