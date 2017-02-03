(*  Book: Modern Compiler Implementation in ML

Write an ML function interp : stm→unit that “interprets” a program in this language. 
To write in a “functional” style – without [sml] assignment (:=) or arrays – maintain a 
list of (variable,integer) pairs, and produce new versions of this list at each AssignStm.
See the assignment page (http://ccom.uprrp.edu/~humberto/pages/teaching/compilers2017/interpreter.html)	for more details.
	
This filename is: interp.sml	*)

use "slp.sml";

(* Raise when lookup is searching 
for an ID that is not in the table. *)
exception UnboundIdentifier

(* type table = (id * int) list *)
type table = (id * int) list

(* val mtenv = [] : 'a list *)
val mtenv = []

(* Update the linked list. 
	val update = fn : table * id * int -> table *)
fun update (table: table, id: id, value: int ): table = ((id, value)::table);

(* Searches down the Linked List. 
	val lookup = fn : table * id -> int *)
fun lookup (table: table, id: id ): int = 
	case table of [] => raise UnboundIdentifier
	| (firstID, firstNUM)::theEnd => 
	if firstID = id then firstNUM 
	else lookup(theEnd,id);

(* val interp = fn : stm -> unit *)
fun interp s1 = ( interpStm( s1, nil ); () )
	and
	(* val interpExp = fn : exp * table -> int * table *)
	  interpExp ( IdExp id, env ) = ( lookup( env, id ), env )
	| interpExp ( NumExp n, env ) = ( n, env )
	| interpExp ( OpExp ( e1, myop, e2 ), env ) =
		let
			val ( v1, env1 ) = interpExp ( e1, env )
			val ( v2, env2 ) = interpExp ( e2, env1 )
		in
			case myop of
				  Plus 	=> 	( v1 + v2, env2 )
				| Minus => 	( v1 - v2, env2 )
				| Times => 	( v1 * v2, env2 )
				| Div 	=> 	( v1 div v2, env2 )
		end
	| interpExp ( EseqExp ( s1, e1 ), env ) =
		let
				val env1 = interpStm( s1, env )
			in
				interpExp( e1, env1 )
			end	
	and
	(* val interpStm = fn : stm * (id * int) list -> table *) 
	  interpStm ( _, env ) = env;

(*
fun interpstm

fun interexplist*)

(*--------------------------------------------*)
(* Test cases *)
(*--------------------------------------------*)
update ([("B", 4), ("A", 27), ("C", 1)], "B", 1);
update ([("B", 4), ("A", 27)], "C", 1);
update ([("B", 4)], "B", 1);
update ([], "B", 2017);
interpExp(NumExp 5, mtenv);
interpExp(IdExp "a", [("a", 5)]);
interpExp (OpExp (NumExp 3, Plus, (NumExp 5)), mtenv);