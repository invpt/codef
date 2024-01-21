# codef

A programming language with predictable runtime code generation. Currently still in development.

`codef` is a *[multi-stage programming language](https://en.wikipedia.org/wiki/Multi-stage_programming)*. Languages like this allow programmers to write code that can be specialized at runtime. In `codef`'s case, the intermediate representation of all code that must be runtime-specialized is bundled with compiled programs, and part of the compiler is also bundled to compile this IR when it is ready to be specialized. A straightforward use-case of a multi-stage programming language is to turn a simple interpreter into a simple compiler:

## Current status

Current work is around the level of the control-flow graph, working towards having register allocation. Then a simple code generator can be written. After that, a bit of work needs to be done so that the compiler can be linked into applications to allow for the runtime specialization that is characteristic of multi-stage programming.

## Code example

```scala
// Simple AST, parser, and tree-walking interpreter for single-arg functions.
type Expr
    \Add(Expr, Expr)
    \Mul(Expr, Expr)
    \Neg(Expr)
    \Val(Int)
    \Arg;
// Not exactly a parser; not enough string methods to write one right now
def parse(val src :: String) => Expr\Add(Expr\Val 1, Expr\Val 1);
// Evaluates the integer value of the given expression with the given argument
def eval(val expr :: Expr, val arg :: Int) -> Int {
    case expr = Expr\Add(val a, val b) {
        eval(a, arg) + eval(b, arg)
    } else expr = Expr\Mul(val a, val b) {
        eval(a, arg) * eval(b, arg)
    } else expr = Expr\Neg(val a) {
        -eval(a, arg)
    } else expr = Expr\Val(val i) {
        i
    } else /*expr = Expr\Arg*/ { // compiler doesn't know about exhaustiveness right now
        arg
    }
}

// The dollar sign means "do the specialization with these arguments when the function is called"
// Essentially, if you use a dollar sign in the arrow for your function, you're signaling to the
// compiler statically that this is a function you'll use to do runtime specialization.
def compile(val expr :: Expr) $=> (val arg :: Int) => eval(expr, arg);

def main() {
    val src = input();
    val expr = parse(src);
    val calc = compile(expr);

    // Now we can evaluate this function a bunch without paying the interpretation
    // penalty every time!
    var total = 0;
    for var i = 0; i < 1000000000; set i = i + 1 {
        set total = total + calc(i);
    }

    println(itoa(total));
}
```
