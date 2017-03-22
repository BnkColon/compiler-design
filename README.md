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

# Chapter 2: Lexical Analysis

## [Lexical analysis of a Tiger subset](http://ccom.uprrp.edu/~humberto/pages/teaching/compilers2017/lexer.html)

### Description:

Construct a lexer for a subset of tiger that includes the following
```
exp:
    lvalue
    nil
    int
    string
    exp binop exp
    lvalue := exp
    id ( explist )
    ( expseq )
    let declist in expseq end
    if exp then exp
    if exp then exp else exp

lvalue:
    # These are simple variables
    id

binop:
    + - * / = <> < > <= >= & |

declist:
    dec
    dec declist

dec: 
    type id = id
    var id : id := exp
    function id ( typefields ) : id = exp

expseq:
    exp
    expseq ; exp

explist:
    exp
    explist , exp

typefields:
    typefield
    typefield, typefields

typefield:
    id : id
```   
NOTE: the only valid typeid in our language are aliases of the built in "int" and "string" types. We do not have arrays or structures or other compound types.

Use the sample code in [chapter 2](https://www.cs.princeton.edu/~appel/modern/ml/chap2/) to implement (in tiger.lex) a lexer for this subset of tiger. The full description of tiger is in the Appendix A of the book, and you can get a [short description of Tiger](http://www.cs.columbia.edu/~sedwards/classes/2002/w4115/tiger.pdf).

# Chapter 3: Parsing

## [Parser for a Tiger subset](http://ccom.uprrp.edu/~humberto/pages/teaching/compilers2017/subset.html)

### Description

Construct a parser for a subset of tiger that includes the following:

```
exp:
    lvalue
    nil
    int
    string
    exp binop exp
    lvalue := exp
    id ( explist )
    ( expseq )
    let declist in expseq end
    if exp then exp
    if exp then exp else exp

lvalue:
    # These are simple variables
    id

binop:
    + - * / = <> < > <= >= & |

declist:
    dec
    dec declist

dec: 
    type id = id
    var id : id := exp
    function id ( typefields ) : id = exp

expseq:
    exp
    expseq ; exp

explist:
    exp
    explist , exp

typefields:
    typefield
    typefield, typefields

typefield:
    id : id

```
NOTE: the only valid type ids in our language are aliases of the built in "int" and "string" types. We do not have arrays or structures or other compound types.

Your assignment is to use the sample code in [chapter 3](https://www.cs.princeton.edu/~appel/modern/ml/chap3/) to implement (in tiger.grm) a parser for this subset of tiger. The full description of tiger is in the Appendix A of the book, and you can get [a short description of Tiger](http://www.cs.columbia.edu/~sedwards/classes/2002/w4115/tiger.pdf). Remember, you don't have to implement the whole language, just what I describe above.

You will definitely need to read Chapter 3 to see examples of building a parser, and the [ML-Yacc manual](http://www.smlnj.org/doc/ML-Yacc/) is also useful.

Try to keep the number of conflicts in your parser to a minimum. You must not have any reduce-reduce conflicts, and explain any shift-reduce conflicts and how they are resolved.

### Example code

You can parse a sample program using:

```
- CM.make "sources.cm";
- Parse.parse "test.tig";
  () : unit
```

Here's a simple program that should parse.

```
let
  function fac(n : int) : int =
    if n <= 1 then
        1
    else
        n * fac(n - 1)
in
    printi(fac(5))
end
```   

