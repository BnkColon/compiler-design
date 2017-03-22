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

Your assignment is to use the sample code in chapter 3 to implement (in tiger.grm) a parser for this subset of tiger. The full description of tiger is in the Appendix A of the book, and you can get a short description of Tiger. Remember, you don't have to implement the whole language, just what I describe above.

You will definitely need to read Chapter 3 to see examples of building a parser, and the ML-Yacc manual is also useful.

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