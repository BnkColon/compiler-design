# [Parser for a Tiger subset](http://ccom.uprrp.edu/~humberto/pages/teaching/compilers2017/subset.html)

## Description

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

## Example code

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

## Results:

After doing this on the terminal:

```
$ rlwrap sml
Standard ML of New Jersey v110.80 [built: Thu Feb  9 16:24:11 2017]
- CM.make "sources.cm";
[scanning sources.cm]
["/Users/biancacolon/homebrew/Cellar/smlnj/110.80/SMLNJ_HOME/bin/ml-yacc"  "tiger.grm"]
13 shift/reduce conflicts
[parsing (sources.cm):tiger.grm.sig]
[parsing (sources.cm):tiger.grm.sml]
[compiling (sources.cm):tiger.grm.sig]
[code: 56, env: 980 bytes]
[compiling (sources.cm):tiger.grm.sml]
[code: 39132, data: 2960, env: 1146 bytes]
[New bindings added.]
val it = true : bool
- Parse.parse "test.tig";
val it = () : unit
```

We can see this results in the tiger.grm.desc file: 

```
13 shift/reduce conflicts

error:  state 61: shift/reduce conflict (shift ELSE, reduce by rule 22)
error:  state 77: shift/reduce conflict (shift OR, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift AND, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift GE, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift GT, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift LE, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift LT, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift NEQ, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift EQ, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift DIVIDE, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift TIMES, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift MINUS, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift PLUS, reduce by rule 23)

error:  state 61: shift/reduce conflict (shift ELSE, reduce by rule 22)

state 61:

    exp : exp . PLUS exp 
    exp : exp . MINUS exp 
    exp : exp . TIMES exp 
    exp : exp . DIVIDE exp 
    exp : exp . EQ exp 
    exp : exp . NEQ exp 
    exp : exp . LT exp 
    exp : exp . LE exp 
    exp : exp . GT exp 
    exp : exp . GE exp 
    exp : exp . AND exp 
    exp : exp . OR exp 
    exp : IF exp THEN exp .  (reduce by rule 22)
    exp : IF exp THEN exp . ELSE exp 

    PLUS    shift 23
    MINUS   shift 22
    TIMES   shift 21
    DIVIDE  shift 20
    EQ  shift 19
    NEQ shift 18
    LT  shift 17
    LE  shift 16
    GT  shift 15
    GE  shift 14
    AND shift 13
    OR  shift 12
    ELSE    shift 71


    .   reduce by rule 22


error:  state 77: shift/reduce conflict (shift OR, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift AND, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift GE, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift GT, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift LE, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift LT, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift NEQ, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift EQ, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift DIVIDE, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift TIMES, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift MINUS, reduce by rule 23)
error:  state 77: shift/reduce conflict (shift PLUS, reduce by rule 23)

state 77:

    exp : exp . PLUS exp 
    exp : exp . MINUS exp 
    exp : exp . TIMES exp 
    exp : exp . DIVIDE exp 
    exp : exp . EQ exp 
    exp : exp . NEQ exp 
    exp : exp . LT exp 
    exp : exp . LE exp 
    exp : exp . GT exp 
    exp : exp . GE exp 
    exp : exp . AND exp 
    exp : exp . OR exp 
    exp : IF exp THEN exp ELSE exp .  (reduce by rule 23)

    PLUS    shift 23
    MINUS   shift 22
    TIMES   shift 21
    DIVIDE  shift 20
    EQ  shift 19
    NEQ shift 18
    LT  shift 17
    LE  shift 16
    GT  shift 15
    GE  shift 14
    AND shift 13
    OR  shift 12


    .   reduce by rule 23   

```

This are the states that have shift/reduce conflicts and now we can see in what instructions they are generated.