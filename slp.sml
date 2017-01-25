(*Write a program to compute the maximum number of arguments in 
a straight line program as described in Chapter 1. 
See the assignment page (http://ccom.uprrp.edu/~humberto/pages/teaching/compilers2017/expressions.html) 
for more details.*)

type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list

  and exp = IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

fun maxargs (PrintStm args) = length args
  | maxargs (AssignStm (_, e)) = maxargs_exp e
  | maxargs (CompoundStm (s1, s2)) = Int.max(maxargs s1, maxargs s2)
  and maxargs_exp (IdExp _) = 0
  | maxargs_exp (NumExp _) = 0
  | maxargs_exp (OpExp (e1, _, e2)) = Int.max(maxargs_exp e1, maxargs_exp e2)
  | maxargs_exp (EseqExp (s, e)) = Int.max(maxargs s, maxargs_exp e);

(* Book example code. *) (* Result: val it = 2 : int *)
val prog = CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b", EseqExp(PrintStm[IdExp"a", OpExp(IdExp"a", Minus,NumExp 1)],
  OpExp(NumExp 10, Times, IdExp"a"))), PrintStm[IdExp "b"])); 


(*Assignment Example: a := (print (1, 2, 3, 4), 2) ; print ( a + 1 )  *)  (* Result: val it = 4 : int *)
val value = CompoundStm(AssignStm("a", EseqExp(PrintStm[NumExp 1, NumExp 2, NumExp 3, NumExp 4], NumExp 2)), 
  PrintStm[OpExp(IdExp"a", Plus, NumExp 1)]);


maxargs prog;
maxargs value; 
