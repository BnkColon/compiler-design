(* 
Write an ML function interp : stm→unit that “interprets” a program in this language. 
To write in a “functional” style – without [sml] assignment (:=) or arrays – maintain a 
list of (variable,integer) pairs, and produce new versions of this list at each AssignStm.
See the assignment page (http://ccom.uprrp.edu/~humberto/pages/teaching/compilers2017/interpreter.html)	for more details.
	
This filename is: interp.sml	

*)
use "slp.sml";

type table = (id * int) list

fun update(l: table, id: id,value: int): table = ((id, value)::l);

fun lookup (l: table, id: id): int =
(*
fun interp

fun interpexp

fun interpstm

fun interexplist*)