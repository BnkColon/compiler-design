type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

val str = ref ""
val strPos  = ref 0; 
val commentCounter  = ref 0;

val unclosedString = ref false

fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in 
	if !commentCounter > 0 
		then (ErrorMsg.error pos ("Unclosed comment! Fix it."); commentCounter := 0; Tokens.EOF(pos,pos))
	else if !unclosedString = true
		then (ErrorMsg.error pos ("Unclosed string! Fix it."); Tokens.EOF(pos,pos))
	else (Tokens.EOF(pos,pos)) end

%%
%s STRING COMMENT;  
digits = [0-9]+; 
letters = [A-za-z]+;
%%
<INITIAL>\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>\t => (continue());


<INITIAL>","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL>";" => (Tokens.SEMICOLON(yypos, yypos+1));
<INITIAL>":="=> (Tokens.ASSIGN(yypos, yypos+2));
<INITIAL>":" => (Tokens.COLON(yypos, yypos+1));

<INITIAL>"+" => (Tokens.PLUS(yypos, yypos+1));
<INITIAL>"-" => (Tokens.MINUS(yypos, yypos+1));
<INITIAL>"*" => (Tokens.PLUS(yypos, yypos+1));
<INITIAL>"/" => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>"=" => (Tokens.EQ(yypos, yypos+1));
<INITIAL>"<>"=> (Tokens.NEQ(yypos, yypos+2));
<INITIAL>"<" => (Tokens.LT(yypos, yypos+1));
<INITIAL>">" => (Tokens.GT(yypos, yypos+1));
<INITIAL>"<="=> (Tokens.LE(yypos, yypos+2));
<INITIAL>">="=> (Tokens.GE(yypos, yypos+2));
<INITIAL>"&" => (Tokens.AND(yypos, yypos+1));
<INITIAL>"|" => (Tokens.OR(yypos, yypos+1));

<INITIAL>" " => (continue());

<INITIAL>"(" => (Tokens.LPAREN(yypos, yypos+1));
<INITIAL>")" => (Tokens.RPAREN(yypos, yypos+1));
<INITIAL>"{" => (Tokens.LBRACE(yypos, yypos+1));
<INITIAL>"}" => (Tokens.RBRACE(yypos, yypos+1));
<INITIAL>"[" => (Tokens.LBRACK(yypos, yypos+1));
<INITIAL>"]" => (Tokens.RBRACK(yypos, yypos+1));

<INITIAL>nil => (Tokens.NIL(yypos, yypos+3));
<INITIAL>if  => (Tokens.IF(yypos, yypos+2));
<INITIAL>then=> (Tokens.THEN(yypos, yypos+4));
<INITIAL>else=> (Tokens.ELSE(yypos, yypos+4));
<INITIAL>let => (Tokens.LET(yypos, yypos+3));
<INITIAL>in  => (Tokens.IN(yypos, yypos+2));
<INITIAL>end => (Tokens.END(yypos, yypos+3));

<INITIAL>function 	=> (Tokens.FUNCTION(yypos, yypos+8));
<INITIAL>var 		=> (Tokens.VAR(yypos, yypos+3));
<INITIAL>type 		=> (Tokens.TYPE(yypos, yypos+4));

<INITIAL>{letters}	=> (Tokens.ID(yytext, yypos, yypos+(String.size yytext)));
<INITIAL>{digits}	=> (let 
							val SOME x = Int.fromString yytext 
						in 
							Tokens.INT(x, yypos, yypos+(String.size yytext))
						end);

<INITIAL>"\""	=> (YYBEGIN STRING; str := ""; strPos := yypos; unclosedString := true; continue());
<STRING>\"    	=> (YYBEGIN INITIAL; unclosedString := false; Tokens.STRING(!str, !strPos, yypos+1));
<STRING>\\(n|t|\^c|[0-9]{3}|\"|\\|" ")	=> (str := !str ^ valOf(String.fromString yytext); continue());
<STRING>.      	=> (str := !str ^ yytext; continue());

<INITIAL>"/*" 	=> (YYBEGIN COMMENT; commentCounter := !commentCounter+1; continue()); 
<COMMENT>"/*" 	=> (commentCounter := !commentCounter + 1; continue()); 
<COMMENT>"*/" 	=> (commentCounter := !commentCounter - 1; continue()); 
<COMMENT>[\n] 	=> (lineNum := !lineNum+1; linePos := yypos+1 :: !linePos; continue());
<COMMENT>. 		=> (continue());

<INITIAL>.      => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

