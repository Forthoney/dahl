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
  datatype bin_op =
    Plus | Minus | Mult | Div | Mod | Pow
  | Concat
  | Eq | Neq | Lt | Gt | Lte | Gte
  | And | Or

  type name = string

  datatype exp =
    ENil
  | EFalse
  | ETrue
  | ENumber of string
  | EString of string
  | EVararg
  | EUnary of un_op * exp
  | EBinary of exp * bin_op * exp
  | EPrefix of prefix_exp

  and var =
    VName of name
  | VIndex of prefix_exp * exp
  | VField of prefix_exp * name

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

  and laststat = Return of exp list | Break

  val unOpToString =
    fn Neg => "~"
     | Not => "!"
     | Length => "#"

  val binOpToString =
    fn Plus => "+"
     | Minus => "-"
     | Mult => "*"
     | Div => "/"
     | Mod => "%"
     | Pow => "^"
     | Concat => "@"
     | Eq => "="
     | Neq => "!="
     | Lt => "<"
     | Gt => ">"
     | Lte => "<="
     | Gte => ">="
     | And => "&&"
     | Or => "||"

  structure SExpFrom =
  struct
    structure S = SExp

    fun exp e =
      case e of
        ENil => S.Atom "nil"
      | EFalse => S.Atom "false"
      | ETrue => S.Atom "true"
      | ENumber s => S.List [S.Atom "ENumber", S.Atom s]
      | EString s => S.Atom ("\"" ^ String.toString s ^ "\"")
      | EVararg => S.Atom "<vararg>"
      | EUnary (opr, e) => S.List [S.Atom (unOpToString opr), exp e]
      | EBinary (lhs, opr, rhs) => S.List [exp lhs, S.Atom (binOpToString opr), exp rhs]
      | EPrefix pfx => prefix pfx

    and prefix pfx =
      case pfx of
        PVar v => var v
      | PFnCall fcall => funcCall fcall
      | PExp e => exp e

    and var v =
      case v of
        VName n => S.Atom n
      | VIndex (pfx, e) => S.namedStruct ("index", [prefix pfx, exp e])
      | VField (pfx, n) => S.List [S.Atom ("." ^ n), prefix pfx]

    and funcCall (Function (pfx, args)) =
        S.namedStruct ("fcall", [prefix pfx, funcArgs args])
      | funcCall (Method (pfx, name, args)) =
        S.namedStruct ("mcall", [prefix pfx, S.Atom name, funcArgs args])

    and funcArgs args =
      case args of
        AExp exps => S.List (map exp exps)
      | ATable tbl => S.Atom "table"
      | AString s => S.Atom ("\"" ^ String.toString s ^ "\"")

    fun stmt node =
      case node of
        LocalAssign (names, exps) =>
          S.namedStruct ("local-assign", [S.List (map S.Atom names), S.List (map exp exps)])
      | LocalFunction (name, {params, vararg, block = blk}) =>
        let
          val vararg = if vararg then [S.Atom "<vararg>"] else [] 
        in
          S.namedStruct ("local-function", [S.List (map S.Atom params @ vararg), block blk])
        end

    and laststmt (Return exps) = S.namedStruct ("return", map exp exps)
      | laststmt (Break) = S.Atom "break"

    and block (Block (stmts, last)) =
      let
        val l = case last of SOME ls => [laststmt ls] | NONE => []
      in
        S.namedStruct ("block", S.List (map stmt stmts) :: l)
      end
  end
end
