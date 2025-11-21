open AST

%%
%name Lua

%term EOF
    | AND | BREAK | DO | ELSE | ELSEIF | END | FALSE | FOR | FUNCTION
    | IF | IN | LOCAL | NIL | NOT | OR | REPEAT | RETURN | THEN | TRUE
    | UNTIL | WHILE
    | PLUS | MINUS | TIMES | DIV | MOD | POW | LEN
    | EQ | NEQ | LT | LE | GT | GE
    | ASSIGN | LPAREN | RPAREN | LBRACE | RBRACE | LBRACK | RBRACK
    | SEMI | COLON | COMMA | DOT | CONCAT | VARARG
    | NAME of string | STRING of string | NUMBER of real
    | UMINUS

%nonterm chunk of block
       | block of block
       | statlist of stmt list
       | stat of stmt
       | laststat of stmt
       | laststatopt of stmt option
       | funcname of func_name
       | dottednames of string list
       | varlist of var list
       | namelist of string list
       | explist of exp list
       | exp of exp
       | prefixexp of prefix_exp
       | args of exp list
       | functiondef of exp
       | funcbody of func_body
       | parlist of string list * bool
       | tableconstructor of exp
       | fieldlist of field list
       | field of field
       | elseifs of (exp * block) list
       | elseblock of block option

%pos int

%eop EOF
%noshift EOF

%left OR
%left AND
%left LT GT LE GE NEQ EQ
%right CONCAT
%left PLUS MINUS
%left TIMES DIV MOD
%right NOT LEN UMINUS
%right POW

%start chunk

%verbose

%%

chunk : block (block)

block : statlist laststatopt (Block(statlist @ (case laststatopt of SOME s => [s] | NONE => [])))

statlist : (* empty *) ([])
         | statlist stat (statlist @ [stat])
         | statlist stat SEMI (statlist @ [stat])

stat : varlist ASSIGN explist (Assign(varlist, explist))
     | prefixexp (
         case prefixexp of 
             PCall c => CallStmt(c)
           | _ => (print "Syntax Error: unexpected symbol near expression\n"; CallStmt(Call(prefixexp,[])))
       )
     | DO block END (Do(block))
     | WHILE exp DO block END (While(exp, block))
     | REPEAT block UNTIL exp (Repeat(block, exp))
     | IF exp THEN block elseifs elseblock END (If((exp, block) :: elseifs, elseblock))
     | FOR NAME ASSIGN exp COMMA exp DO block END (NumFor(NAME, exp1, exp2, NONE, block))
     | FOR NAME ASSIGN exp COMMA exp COMMA exp DO block END (NumFor(NAME, exp1, exp2, SOME exp3, block))
     | FOR namelist IN explist DO block END (GenFor(namelist, explist, block))
     | FUNCTION funcname funcbody (FuncDef(funcname, funcbody))
     | LOCAL FUNCTION NAME funcbody (LocalFunc(NAME, funcbody))
     | LOCAL namelist (LocalAssign(namelist, []))
     | LOCAL namelist ASSIGN explist (LocalAssign(namelist, explist))

laststatopt : (* empty *) (NONE)
            | laststat (SOME laststat)

laststat : BREAK (Break)
         | RETURN (Return([]))
         | RETURN explist (Return(explist))

elseifs : (* empty *) ([])
        | ELSEIF exp THEN block elseifs ((exp, block) :: elseifs)

elseblock : (* empty *) (NONE)
          | ELSE block (SOME block)

funcname : dottednames (FuncName(dottednames, NONE))
         | dottednames COLON NAME (FuncName(dottednames, SOME NAME))

dottednames : NAME ([NAME])
            | dottednames DOT NAME (dottednames @ [NAME])

varlist : prefixexp (
            case prefixexp of 
                PVar v => [v] 
              | _ => (print "Syntax Error: invalid assignment target\n"; [])
          )
        | varlist COMMA prefixexp (
             case prefixexp of 
                PVar v => varlist @ [v] 
              | _ => (print "Syntax Error: invalid assignment target\n"; varlist)
          )

namelist : NAME ([NAME])
         | namelist COMMA NAME (namelist @ [NAME])

explist : exp ([exp])
        | explist COMMA exp (explist @ [exp])

exp : NIL (Nil)
    | FALSE (Bool(false))
    | TRUE (Bool(true))
    | NUMBER (Number(NUMBER))
    | STRING (String(STRING))
    | VARARG (VarArg)
    | functiondef (functiondef)
    | prefixexp (Prefix(prefixexp))
    | tableconstructor (tableconstructor)
    | exp PLUS exp (BinOp(Plus, exp1, exp2))
    | exp MINUS exp (BinOp(Minus, exp1, exp2))
    | exp TIMES exp (BinOp(Times, exp1, exp2))
    | exp DIV exp (BinOp(Div, exp1, exp2))
    | exp MOD exp (BinOp(Mod, exp1, exp2))
    | exp POW exp (BinOp(Pow, exp1, exp2))
    | exp CONCAT exp (BinOp(Concat, exp1, exp2))
    | exp EQ exp (BinOp(Eq, exp1, exp2))
    | exp NEQ exp (BinOp(Neq, exp1, exp2))
    | exp LT exp (BinOp(Lt, exp1, exp2))
    | exp LE exp (BinOp(Le, exp1, exp2))
    | exp GT exp (BinOp(Gt, exp1, exp2))
    | exp GE exp (BinOp(Ge, exp1, exp2))
    | exp AND exp (BinOp(And, exp1, exp2))
    | exp OR exp (BinOp(Or, exp1, exp2))
    | MINUS exp %prec UMINUS (UnOp(Neg, exp))
    | NOT exp (UnOp(Not, exp))
    | LEN exp (UnOp(Len, exp))

prefixexp : NAME (PVar(Name(NAME)))
          | LPAREN exp RPAREN (PParen(exp))
          | prefixexp args (PCall(Call(prefixexp, args)))
          | prefixexp COLON NAME args (PCall(MethodCall(prefixexp, NAME, args)))
          | prefixexp LBRACK exp RBRACK (PVar(Index(prefixexp, exp)))
          | prefixexp DOT NAME (PVar(Prop(prefixexp, NAME)))

args : LPAREN RPAREN ([])
     | LPAREN explist RPAREN (explist)
     | tableconstructor ([tableconstructor])
     | STRING ([String(STRING)])

functiondef : FUNCTION funcbody (Func(funcbody))

funcbody : LPAREN parlist RPAREN block END (FuncBody(parlist, block))

parlist : (* empty *) ([], false)
        | namelist ((namelist, false))
        | VARARG (([], true))
        | namelist COMMA VARARG ((namelist, true))

tableconstructor : LBRACE fieldlist RBRACE (TableCons(fieldlist))
                 | LBRACE RBRACE (TableCons([]))

fieldlist : field ([field])
          | fieldlist COMMA field (fieldlist @ [field])
          | fieldlist SEMI field (fieldlist @ [field])

field : LBRACK exp RBRACK ASSIGN exp (FieldKey(exp1, exp2))
      | NAME ASSIGN exp (FieldProp(NAME, exp))
      | exp (FieldSeq(exp))
