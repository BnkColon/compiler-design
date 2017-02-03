(* 
Chapter 1: Grammar 1.3 defines a language for straight line programs, 
with expressions and identifiers but no control structures.
  
This file name: slp.sml 
*)

type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list

  and exp = IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

