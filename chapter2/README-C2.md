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