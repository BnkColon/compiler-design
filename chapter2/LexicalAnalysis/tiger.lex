type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


%% 
digits = [0-9]+; 
(*letters = [A-za-z]+;*)
%%
\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
","	=> (Tokens.COMMA(yypos,yypos+1));
var  	=> (Tokens.VAR(yypos,yypos+3));
{digits}=> (let 
				val SOME x = Int.fromString yytext 
			in 
				Tokens.INT(x, yypos, yypos+(String.size yytext))
			end);
.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

