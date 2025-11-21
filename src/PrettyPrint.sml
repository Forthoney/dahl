structure PrettyPrint =
struct
  open AST

  fun sepBy s f xs =
    String.concatWith s (map f xs)

  fun binopStr b =
    case b of
      Plus => "+"
    | Minus => "-"
    | Times => "*"
    | Div => "/"
    | Mod => "%"
    | Pow => "^"
    | Concat => ".."
    | Eq => "=="
    | Neq => "~="
    | Lt => "<"
    | Le => "<="
    | Gt => ">"
    | Ge => ">="
    | And => "and"
    | Or => "or"

  fun unopStr u =
    case u of
      Neg => "-"
    | Not => "not"
    | Len => "#"

  fun expStr e =
    case e of
      Nil => "Nil"
    | Bool b => "Bool(" ^ Bool.toString b ^ ")"
    | Number r => "Number(" ^ Real.toString r ^ ")"
    | String s => "String(\"" ^ String.toString s ^ "\")"
    | VarArg => "..."
    | Func fb => "Func(" ^ funcBodyStr fb ^ ")"
    | Prefix p => "Prefix(" ^ prefixStr p ^ ")"
    | TableCons fields => "TableCons([" ^ sepBy ", " fieldStr fields ^ "])"
    | BinOp (op', e1, e2) =>
        "BinOp(" ^ binopStr op' ^ ", " ^ expStr e1 ^ ", " ^ expStr e2 ^ ")"
    | UnOp (op', e1) => "UnOp(" ^ unopStr op' ^ ", " ^ expStr e1 ^ ")"

  and prefixStr p =
    case p of
      PVar v => "PVar(" ^ varStr v ^ ")"
    | PCall c => "PCall(" ^ callStr c ^ ")"
    | PParen e => "PParen(" ^ expStr e ^ ")"

  and varStr v =
    case v of
      Name n => "Name(" ^ n ^ ")"
    | Index (p, e) => "Index(" ^ prefixStr p ^ ", " ^ expStr e ^ ")"
    | Prop (p, s) => "Prop(" ^ prefixStr p ^ ", ." ^ s ^ ")"

  and callStr c =
    case c of
      Call (p, args) =>
        "Call(" ^ prefixStr p ^ ", [" ^ sepBy ", " expStr args ^ "])"
    | MethodCall (p, m, args) =>
        "MethodCall(" ^ prefixStr p ^ ", :" ^ m ^ ", [" ^ sepBy ", " expStr args
        ^ "])"

  and fieldStr f =
    case f of
      FieldKey (k, v) => "[" ^ expStr k ^ "] = " ^ expStr v
    | FieldProp (k, v) => k ^ " = " ^ expStr v
    | FieldSeq e => expStr e

  and funcBodyStr (FuncBody ((params, isVararg), blk)) =
    let
      val pStr =
        "(" ^ sepBy ", " (fn x => x) params ^ (if isVararg then ", ..." else "")
        ^ ")"
    in
      "FuncBody(" ^ pStr ^ ", " ^ blockStr blk ^ ")"
    end

  and stmtStr s =
    case s of
      Assign (vs, es) =>
        "Assign([" ^ sepBy ", " varStr vs ^ "], [" ^ sepBy ", " expStr es ^ "])"
    | LocalAssign (ns, es) =>
        "LocalAssign([" ^ sepBy ", " (fn x => x) ns ^ "], ["
        ^ sepBy ", " expStr es ^ "])"
    | CallStmt c => "CallStmt(" ^ callStr c ^ ")"
    | Do b => "Do(" ^ blockStr b ^ ")"
    | While (e, b) => "While(" ^ expStr e ^ ", " ^ blockStr b ^ ")"
    | Repeat (b, e) => "Repeat(" ^ blockStr b ^ ", " ^ expStr e ^ ")"
    | If (conds, elseB) =>
        let
          fun condStr (e, b) =
            "(" ^ expStr e ^ ", " ^ blockStr b ^ ")"
          val elseStr =
            case elseB of
              NONE => "NONE"
            | SOME b => "SOME(" ^ blockStr b ^ ")"
        in
          "If([" ^ sepBy ", " condStr conds ^ "], " ^ elseStr ^ ")"
        end
    | NumFor (n, e1, e2, e3, b) =>
        let
          val step =
            case e3 of
              NONE => "NONE"
            | SOME e => "SOME(" ^ expStr e ^ ")"
        in
          "NumFor(" ^ n ^ ", " ^ expStr e1 ^ ", " ^ expStr e2 ^ ", " ^ step
          ^ ", " ^ blockStr b ^ ")"
        end
    | GenFor (ns, es, b) =>
        "GenFor([" ^ sepBy ", " (fn x => x) ns ^ "], [" ^ sepBy ", " expStr es
        ^ "], " ^ blockStr b ^ ")"
    | FuncDef (FuncName (parts, method), body) =>
        let
          val name =
            String.concatWith "." parts
            ^
            (case method of
               NONE => ""
             | SOME m => ":" ^ m)
        in
          "FuncDef(" ^ name ^ ", " ^ funcBodyStr body ^ ")"
        end
    | LocalFunc (n, body) => "LocalFunc(" ^ n ^ ", " ^ funcBodyStr body ^ ")"
    | Break => "Break"
    | Return es => "Return([" ^ sepBy ", " expStr es ^ "])"

  and blockStr (Block stmts) =
    "Block[\n  " ^ sepBy ",\n  " stmtStr stmts ^ "\n]"

  fun printTree (Block stmts) =
    print (blockStr (Block stmts) ^ "\n")
end
