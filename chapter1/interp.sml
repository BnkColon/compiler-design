(* 
Write an ML function interp : stm→unit that “interprets” a program in this language. 
To write in a “functional” style – without [sml] assignment (:=) or arrays – maintain a 
list of (variable,integer) pairs, and produce new versions of this list at each AssignStm.
See the assignment page (http://ccom.uprrp.edu/~humberto/pages/teaching/compilers2017/interpreter.html)	for more details.
	
This filename is: interp.sml	
*)

use "slp.sml";

(* Raise when lookup is searching 
for an ID that is not in the table. *)
exception UnboundIdentifier

(* type table = (id * int) list *)
type table = (id * int) list

(* val mtenv = [] : 'a list *)
val mtenv = []

(* Update the linked list. 
	val update = fn : table * id * int -> table
*)
fun update(table: table, id: id,value: int): table = ((id, value)::table);

(* Searches down the Linked List. 
	val lookup = fn : table * id -> int
*)
fun lookup (table: table, id: id): int = 
	case table of [] => raise UnboundIdentifier
	| (firstID, firstNUM)::theEnd => 
	if firstID = id then firstNUM 
	else lookup(theEnd,id);
(*
fun interp
*)

fun interpexp


(*
fun interpstm

fun interexplist*)