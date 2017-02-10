type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


%% 
digits = [0-9]+; 
letters = [A-za-z]+;
%%
\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
\t  => (continue());

","	=> (Tokens.COMMA(yypos,yypos+1));
";" => (Tokens.SEMICOLON(yypos, yypos+1));
":="=> (Tokens.ASSIGN(yypos, yypos+2));
":" => (Tokens.COLON(yypos, yypos+1));

"+" => (Tokens.PLUS(yypos, yypos+1));
"-" => (Tokens.MINUS(yypos, yypos+1));
"*" => (Tokens.PLUS(yypos, yypos+1));
"/" => (Tokens.DIVIDE(yypos,yypos+1));
"=" => (Tokens.EQ(yypos, yypos+1));
"<>"=> (Tokens.NEQ(yypos, yypos+2));
"<" => (Tokens.LT(yypos, yypos+1));
">" => (Tokens.GT(yypos, yypos+1));
"<="=> (Tokens.LE(yypos, yypos+2));
">="=> (Tokens.GE(yypos, yypos+2));
"&" => (Tokens.AND(yypos, yypos+1));
"|" => (Tokens.OR(yypos, yypos+1));

" " => (continue());

"(" => (Tokens.LPAREN(yypos, yypos+1));
")" => (Tokens.RPAREN(yypos, yypos+1));
"{" => (Tokens.LBRACE(yypos, yypos+1));
"}" => (Tokens.RBRACE(yypos, yypos+1));
"[" => (Tokens.LBRACK(yypos, yypos+1));
"]" => (Tokens.RBRACK(yypos, yypos+1));

nil => (Tokens.NIL(yypos, yypos+3));
if  => (Tokens.IF(yypos, yypos+2));
then=> (Tokens.THEN(yypos, yypos+4));
else=> (Tokens.ELSE(yypos, yypos+4));
let => (Tokens.LET(yypos, yypos+3));
in  => (Tokens.IN(yypos, yypos+2));
end => (Tokens.END(yypos, yypos+3));

function => (Tokens.FUNCTION(yypos, yypos+8));
var => (Tokens.VAR(yypos, yypos+3));
type => (Tokens.TYPE(yypos, yypos+4));

{letters}	=> (Tokens.ID(yytext, yypos, yypos+(String.size yytext)));
{digits}	=> (let 
					val SOME x = Int.fromString yytext 
				in 
					Tokens.INT(x, yypos, yypos+(String.size yytext))
				end);

.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

