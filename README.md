# CCOM 4087 - Compiler Design - Spring 2017

Description: Introduction to compiling, structure of simple one-step compilers: syntax and lexical analysis, parsing, introduction to type checking, intermediate code generation, introduction to code generation and optimization. Discussion about tools for compilers design (e.g. Lex and Yacc).

[More details](http://ccom.uprrp.edu/~humberto/pages/teaching/compilers2017.html)

# Chapter 1: Introduction

## [Straight line programs](http://ccom.uprrp.edu/~humberto/pages/teaching/compilers2017/expressions.html)

### Description

Grammar 1.3 defines a language for straight line programs, with expressions and identifiers but no control structures.

```
Stm -> Stm ; Stm            (* CompoundStm *)
Stm -> id := Exp            (* AssignStm   *)
Stm -> print ( ExpList )    (* PrintStm    *)
Exp -> id                   (* IdExp       *)
Exp -> num                  (* NumExp      *)
Exp -> Exp Binop Exp        (* OpExp       *)
Exp -> ( Stm , Exp )        (* EseqExp     *)
ExpList -> Exp , ExpList    (* exp list    *)
ExpList -> Exp
Binop -> +
Binop -> -
Binop -> *
Binop -> /
```
See the [example code for chapter 1] for data structures to represent programs in this language.

Your first assignment is to implement a function to count the maximum number of arguments to print in a statement (including any subexpressions).
Be careful, you could have a program like:
```
a := (print (1, 2, 3, 4), 2) ; print ( a + 1 )
```
The first print statement has 4 arguments, so your program should return 4 for this example.

Remember to trust the recursion.

Here are a few test cases, I think they all should return 3. What does your program return?

```
val outer3 = PrintStm([EseqExp(PrintStm[NumExp 1, NumExp 2], NumExp 1), 
NumExp 2, NumExp 3]);

val left3 = PrintStm([EseqExp(PrintStm[NumExp 1, NumExp 2, NumExp 3], NumExp 1), 
NumExp 2]);

val right3 = PrintStm([NumExp 1, EseqExp(PrintStm[NumExp 1, NumExp 2, NumExp 3], NumExp 2)]);
```

## [Interpreter for SLP](http://ccom.uprrp.edu/~humberto/pages/teaching/compilers2017/interpreter.html)

### Description

The second programming assigment in chapter 1 is

> > Write an ML function interp : stm→unit that “interprets” a program in this language. To write in a “functional” style – without [sml] assignment (:=) or arrays – maintain a list of (variable,integer) pairs, and produce new versions of this list at each AssignStm.

Appel suggests making two mutually recursive funcions:

```
val interpStm = fn : stm * table -> table
val interpExp = fn : exp * table -> int * table
```
These thread a table, a list of mappings from identifiers to values, through the interpreter to keep track of the assignment statements:

```
type table = (id * int) list
```
Then you can write functions lookup and update to update the table on assignments, and lookup the current value of an identifier.
```
val lookup = fn : table * id -> int
val update = fn : table * id * int -> table
```

