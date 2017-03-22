functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\011\000\003\000\010\000\004\000\009\000\008\000\008\000\
\\029\000\007\000\036\000\006\000\041\000\005\000\000\000\
\\001\000\002\000\039\000\000\000\
\\001\000\002\000\040\000\000\000\
\\001\000\002\000\041\000\000\000\
\\001\000\002\000\056\000\000\000\
\\001\000\002\000\057\000\000\000\
\\001\000\002\000\060\000\000\000\
\\001\000\002\000\072\000\000\000\
\\001\000\002\000\073\000\000\000\
\\001\000\005\000\054\000\009\000\053\000\000\000\
\\001\000\006\000\049\000\000\000\
\\001\000\006\000\067\000\000\000\
\\001\000\006\000\071\000\000\000\
\\001\000\007\000\044\000\009\000\043\000\000\000\
\\001\000\007\000\044\000\038\000\055\000\000\000\
\\001\000\008\000\050\000\000\000\
\\001\000\009\000\066\000\000\000\
\\001\000\015\000\025\000\016\000\024\000\017\000\023\000\018\000\022\000\
\\019\000\021\000\020\000\020\000\021\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\025\000\015\000\026\000\014\000\
\\030\000\042\000\000\000\
\\001\000\019\000\048\000\000\000\
\\001\000\019\000\074\000\000\000\
\\001\000\027\000\064\000\000\000\
\\001\000\037\000\038\000\000\000\
\\001\000\042\000\030\000\043\000\029\000\044\000\028\000\000\000\
\\077\000\015\000\025\000\016\000\024\000\017\000\023\000\018\000\022\000\
\\019\000\021\000\020\000\020\000\021\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\025\000\015\000\026\000\014\000\000\000\
\\078\000\027\000\012\000\000\000\
\\079\000\000\000\
\\080\000\000\000\
\\081\000\000\000\
\\082\000\015\000\025\000\016\000\024\000\017\000\023\000\018\000\022\000\
\\019\000\021\000\020\000\020\000\021\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\025\000\015\000\026\000\014\000\000\000\
\\083\000\015\000\025\000\016\000\024\000\017\000\023\000\018\000\022\000\
\\019\000\021\000\020\000\020\000\021\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\025\000\015\000\026\000\014\000\000\000\
\\084\000\000\000\
\\085\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\015\000\025\000\016\000\024\000\017\000\023\000\018\000\022\000\
\\019\000\021\000\020\000\020\000\021\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\025\000\015\000\026\000\014\000\
\\031\000\062\000\000\000\
\\089\000\015\000\025\000\016\000\024\000\017\000\023\000\018\000\022\000\
\\019\000\021\000\020\000\020\000\021\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\025\000\015\000\026\000\014\000\000\000\
\\090\000\008\000\034\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\042\000\030\000\043\000\029\000\044\000\028\000\000\000\
\\104\000\000\000\
\\105\000\000\000\
\\106\000\015\000\025\000\016\000\024\000\017\000\023\000\018\000\022\000\
\\019\000\021\000\020\000\020\000\021\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\025\000\015\000\026\000\014\000\000\000\
\\107\000\015\000\025\000\016\000\024\000\017\000\023\000\018\000\022\000\
\\019\000\021\000\020\000\020\000\021\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\025\000\015\000\026\000\014\000\000\000\
\\108\000\015\000\025\000\016\000\024\000\017\000\023\000\018\000\022\000\
\\019\000\021\000\020\000\020\000\021\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\025\000\015\000\026\000\014\000\000\000\
\\109\000\015\000\025\000\016\000\024\000\017\000\023\000\018\000\022\000\
\\019\000\021\000\020\000\020\000\021\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\025\000\015\000\026\000\014\000\000\000\
\\110\000\015\000\025\000\016\000\024\000\017\000\023\000\018\000\022\000\
\\019\000\021\000\020\000\020\000\021\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\025\000\015\000\026\000\014\000\000\000\
\\111\000\015\000\025\000\016\000\024\000\017\000\023\000\018\000\022\000\
\\019\000\021\000\020\000\020\000\021\000\019\000\022\000\018\000\
\\023\000\017\000\024\000\016\000\025\000\015\000\026\000\014\000\000\000\
\\112\000\005\000\065\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\"
val actionRowNumbers =
"\001\000\025\000\024\000\026\000\
\\023\000\001\000\001\000\028\000\
\\027\000\037\000\001\000\001\000\
\\049\000\048\000\047\000\046\000\
\\045\000\044\000\043\000\042\000\
\\041\000\040\000\039\000\038\000\
\\050\000\022\000\002\000\003\000\
\\004\000\018\000\014\000\055\000\
\\001\000\030\000\029\000\051\000\
\\001\000\019\000\011\000\016\000\
\\001\000\032\000\001\000\010\000\
\\057\000\015\000\005\000\006\000\
\\007\000\035\000\056\000\031\000\
\\001\000\033\000\052\000\021\000\
\\059\000\017\000\012\000\034\000\
\\001\000\058\000\001\000\007\000\
\\013\000\008\000\036\000\053\000\
\\060\000\009\000\061\000\020\000\
\\001\000\054\000\000\000"
val gotoT =
"\
\\001\000\002\000\002\000\074\000\004\000\001\000\000\000\
\\000\000\
\\005\000\011\000\000\000\
\\000\000\
\\007\000\025\000\008\000\024\000\000\000\
\\001\000\029\000\004\000\001\000\000\000\
\\001\000\031\000\004\000\001\000\006\000\030\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\033\000\004\000\001\000\000\000\
\\001\000\034\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\035\000\008\000\024\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\011\000\000\000\
\\000\000\
\\005\000\011\000\000\000\
\\001\000\044\000\003\000\043\000\004\000\001\000\000\000\
\\005\000\011\000\000\000\
\\005\000\011\000\000\000\
\\000\000\
\\001\000\031\000\004\000\001\000\006\000\045\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\049\000\004\000\001\000\000\000\
\\000\000\
\\001\000\050\000\004\000\001\000\000\000\
\\000\000\
\\005\000\011\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\057\000\010\000\056\000\000\000\
\\005\000\011\000\011\000\059\000\000\000\
\\005\000\011\000\000\000\
\\000\000\
\\001\000\061\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\066\000\004\000\001\000\000\000\
\\005\000\011\000\000\000\
\\001\000\067\000\004\000\001\000\000\000\
\\009\000\068\000\010\000\056\000\000\000\
\\000\000\
\\000\000\
\\005\000\011\000\000\000\
\\005\000\011\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\073\000\004\000\001\000\000\000\
\\005\000\011\000\000\000\
\\000\000\
\"
val numstates = 75
val numrules = 38
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string)
end
type svalue = MlyValue.svalue
type result = unit
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 31) => true | (T 32) => true | (T 33) => true | (T 39) => true
 | (T 35) => true | (T 36) => true | (T 37) => true | (T 41) => true
 | (T 42) => true | (T 43) => true | (T 27) => true | (T 28) => true
 | (T 29) => true | (T 30) => true | (T 34) => true | (T 38) => true
 | (T 40) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 29))::
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "TIMES"
  | (T 17) => "DIVIDE"
  | (T 18) => "EQ"
  | (T 19) => "NEQ"
  | (T 20) => "LT"
  | (T 21) => "LE"
  | (T 22) => "GT"
  | (T 23) => "GE"
  | (T 24) => "AND"
  | (T 25) => "OR"
  | (T 26) => "ASSIGN"
  | (T 27) => "ARRAY"
  | (T 28) => "IF"
  | (T 29) => "THEN"
  | (T 30) => "ELSE"
  | (T 31) => "WHILE"
  | (T 32) => "FOR"
  | (T 33) => "TO"
  | (T 34) => "DO"
  | (T 35) => "LET"
  | (T 36) => "IN"
  | (T 37) => "END"
  | (T 38) => "OF"
  | (T 39) => "BREAK"
  | (T 40) => "NIL"
  | (T 41) => "FUNCTION"
  | (T 42) => "VAR"
  | (T 43) => "TYPE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37)
 $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30)
 $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23)
 $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16)
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.ntVOID lvalue1, lvalue1left, lvalue1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 lvalue1 = lvalue1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, lvalue1left, lvalue1right), rest671)
end
|  ( 2, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 0, ( result, NIL1left, NIL1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.ntVOID (fn _ => ( let val  INT1 = INT1
 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, INT1left, INT1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.STRING STRING1, STRING1left, STRING1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 STRING1 = STRING1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, STRING1left, STRING1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: ( _, ( 
MlyValue.ntVOID binop1, _, _)) :: ( _, ( MlyValue.ntVOID exp1, 
exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  exp1 = exp1 ()
 val  binop1 = binop1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ntVOID lvalue1, lvalue1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  lvalue1 = lvalue1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, lvalue1left, exp1right), rest671)
end
|  ( 7, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID 
explist1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1
 = ID1 ()
 val  explist1 = explist1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 8, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID 
expseq1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val 
 result = MlyValue.ntVOID (fn _ => ( let val  expseq1 = expseq1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 9, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.ntVOID expseq1,
 _, _)) :: _ :: ( _, ( MlyValue.ntVOID declist1, _, _)) :: ( _, ( _, 
LET1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  declist1 = declist1 ()
 val  expseq1 = expseq1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ntVOID expelse1, _, expelse1right)) :: ( _,
 ( MlyValue.ntVOID exp2, _, _)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _
, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  expelse1 = expelse1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, IF1left, expelse1right), rest671)
end
|  ( 11, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 10, ( result, defaultPos, defaultPos), rest671)
end
|  ( 12, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: ( _, ( _, 
ELSE1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 10, ( result, ELSE1left, exp1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, ID1left, ID1right), rest671)
end
|  ( 14, ( ( _, ( _, PLUS1left, PLUS1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 4, ( result, PLUS1left, PLUS1right), rest671)
end
|  ( 15, ( ( _, ( _, MINUS1left, MINUS1right)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 4, ( result, MINUS1left, MINUS1right), rest671)
end
|  ( 16, ( ( _, ( _, TIMES1left, TIMES1right)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 4, ( result, TIMES1left, TIMES1right), rest671)
end
|  ( 17, ( ( _, ( _, DIVIDE1left, DIVIDE1right)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 4, ( result, DIVIDE1left, DIVIDE1right), rest671)
end
|  ( 18, ( ( _, ( _, EQ1left, EQ1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 4, ( result, EQ1left, EQ1right), rest671)
end
|  ( 19, ( ( _, ( _, NEQ1left, NEQ1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 4, ( result, NEQ1left, NEQ1right), rest671)
end
|  ( 20, ( ( _, ( _, LT1left, LT1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 4, ( result, LT1left, LT1right), rest671)
end
|  ( 21, ( ( _, ( _, LE1left, LE1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 4, ( result, LE1left, LE1right), rest671)
end
|  ( 22, ( ( _, ( _, GT1left, GT1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 4, ( result, GT1left, GT1right), rest671)
end
|  ( 23, ( ( _, ( _, GE1left, GE1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 4, ( result, GE1left, GE1right), rest671)
end
|  ( 24, ( ( _, ( _, AND1left, AND1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 4, ( result, AND1left, AND1right), rest671)
end
|  ( 25, ( ( _, ( _, OR1left, OR1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 4, ( result, OR1left, OR1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.ntVOID dec1, dec1left, dec1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
dec1 = dec1 ()
 in ()
end; ()))
 in ( LrTable.NT 6, ( result, dec1left, dec1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.ntVOID declist1, _, declist1right)) :: ( _,
 ( MlyValue.ntVOID dec1, dec1left, _)) :: rest671)) => let val  result
 = MlyValue.ntVOID (fn _ => ( let val  dec1 = dec1 ()
 val  declist1 = declist1 ()
 in ()
end; ()))
 in ( LrTable.NT 6, ( result, dec1left, declist1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in ()
end; ()))
 in ( LrTable.NT 7, ( result, TYPE1left, ID2right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _,
 ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 7, ( result, VAR1left, exp1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, _, _)) :: _ :: _ :: ( _, ( MlyValue.ntVOID 
typefields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _
, FUNCTION1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  ID1 = ID1 ()
 val  typefields1 = typefields1 ()
 val  ID2 = ID2 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 7, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 5, ( result, exp1left, exp1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ntVOID expseq1, expseq1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  expseq1 = expseq1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 5, ( result, expseq1left, exp1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, exp1left, exp1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ntVOID explist1, explist1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  explist1 = explist1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, explist1left, exp1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.ntVOID typefield1, typefield1left, 
typefield1right)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  typefield1 = typefield1 ()
 in ()
end; ()))
 in ( LrTable.NT 8, ( result, typefield1left, typefield1right), 
rest671)
end
|  ( 36, ( ( _, ( MlyValue.ntVOID typefields1, _, typefields1right))
 :: _ :: ( _, ( MlyValue.ntVOID typefield1, typefield1left, _)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
typefield1 = typefield1 ()
 val  typefields1 = typefields1 ()
 in ()
end; ()))
 in ( LrTable.NT 8, ( result, typefield1left, typefields1right), 
rest671)
end
|  ( 37, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in ()
end; ()))
 in ( LrTable.NT 9, ( result, ID1left, ID2right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
end
end
