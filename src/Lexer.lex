type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

val pos = ref 1
val error = fn (x,p) => TextIO.output(TextIO.stdOut, "Lexer error: " ^ x ^ " at line " ^ (Int.toString p) ^ "\n")
val eof = fn () => Tokens.EOF(!pos,!pos)

fun inc_pos n = pos := !pos + n

%%
%header (functor LuaLexFun(structure Tokens: Lua_TOKENS));
%s COMMENT STRING;
digit=[0-9];
alpha=[a-zA-Z_];
ws=[\ \t];

%%

<INITIAL>\n       => (inc_pos 1; lex());
<INITIAL>{ws}+    => (lex());

<INITIAL>"and"    => (Tokens.AND(!pos,!pos));
<INITIAL>"break"  => (Tokens.BREAK(!pos,!pos));
<INITIAL>"do"     => (Tokens.DO(!pos,!pos));
<INITIAL>"else"   => (Tokens.ELSE(!pos,!pos));
<INITIAL>"elseif" => (Tokens.ELSEIF(!pos,!pos));
<INITIAL>"end"    => (Tokens.END(!pos,!pos));
<INITIAL>"false"  => (Tokens.FALSE(!pos,!pos));
<INITIAL>"for"    => (Tokens.FOR(!pos,!pos));
<INITIAL>"function" => (Tokens.FUNCTION(!pos,!pos));
<INITIAL>"if"     => (Tokens.IF(!pos,!pos));
<INITIAL>"in"     => (Tokens.IN(!pos,!pos));
<INITIAL>"local"  => (Tokens.LOCAL(!pos,!pos));
<INITIAL>"nil"    => (Tokens.NIL(!pos,!pos));
<INITIAL>"not"    => (Tokens.NOT(!pos,!pos));
<INITIAL>"or"     => (Tokens.OR(!pos,!pos));
<INITIAL>"repeat" => (Tokens.REPEAT(!pos,!pos));
<INITIAL>"return" => (Tokens.RETURN(!pos,!pos));
<INITIAL>"then"   => (Tokens.THEN(!pos,!pos));
<INITIAL>"true"   => (Tokens.TRUE(!pos,!pos));
<INITIAL>"until"  => (Tokens.UNTIL(!pos,!pos));
<INITIAL>"while"  => (Tokens.WHILE(!pos,!pos));

<INITIAL>"+"      => (Tokens.PLUS(!pos,!pos));
<INITIAL>"-"      => (Tokens.MINUS(!pos,!pos));
<INITIAL>"*"      => (Tokens.TIMES(!pos,!pos));
<INITIAL>"/"      => (Tokens.DIV(!pos,!pos));
<INITIAL>"%"      => (Tokens.MOD(!pos,!pos));
<INITIAL>"^"      => (Tokens.POW(!pos,!pos));
<INITIAL>"#"      => (Tokens.LEN(!pos,!pos));
<INITIAL>"=="     => (Tokens.EQ(!pos,!pos));
<INITIAL>"~="     => (Tokens.NEQ(!pos,!pos));
<INITIAL>"<="     => (Tokens.LE(!pos,!pos));
<INITIAL>">="     => (Tokens.GE(!pos,!pos));
<INITIAL>"<"      => (Tokens.LT(!pos,!pos));
<INITIAL>">"      => (Tokens.GT(!pos,!pos));
<INITIAL>"="      => (Tokens.ASSIGN(!pos,!pos));
<INITIAL>"("      => (Tokens.LPAREN(!pos,!pos));
<INITIAL>")"      => (Tokens.RPAREN(!pos,!pos));
<INITIAL>"{"      => (Tokens.LBRACE(!pos,!pos));
<INITIAL>"}"      => (Tokens.RBRACE(!pos,!pos));
<INITIAL>"["      => (Tokens.LBRACK(!pos,!pos));
<INITIAL>"]"      => (Tokens.RBRACK(!pos,!pos));
<INITIAL>";"      => (Tokens.SEMI(!pos,!pos));
<INITIAL>":"      => (Tokens.COLON(!pos,!pos));
<INITIAL>","      => (Tokens.COMMA(!pos,!pos));
<INITIAL>"."      => (Tokens.DOT(!pos,!pos));
<INITIAL>".."     => (Tokens.CONCAT(!pos,!pos));
<INITIAL>"..."    => (Tokens.VARARG(!pos,!pos));

<INITIAL>\"[^"]*\" => (Tokens.STRING(String.substring(yytext, 1, size(yytext)-2), !pos, !pos));
<INITIAL>\'[^']*\' => (Tokens.STRING(String.substring(yytext, 1, size(yytext)-2), !pos, !pos));
<INITIAL>\[\[[^]]*\]\] => (Tokens.STRING(String.substring(yytext, 2, size(yytext)-4), !pos, !pos));

<INITIAL>"--".* => (lex()); 

<INITIAL>{alpha}({alpha}|{digit})* => (Tokens.NAME(yytext,!pos,!pos));
<INITIAL>{digit}+(\.{digit}+)?([eE][~]?{digit}+)? => (
    case Real.fromString yytext of
        SOME r => Tokens.NUMBER(r, !pos, !pos)
      | NONE => (error("Invalid number", !pos); lex())
);

<INITIAL>.        => (error("Unknown character: " ^ yytext, !pos); lex());
